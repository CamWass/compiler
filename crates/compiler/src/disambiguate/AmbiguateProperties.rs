use super::ColorGraphBuilder::ColorGraphBuilder;
use super::ColorGraphNode::ColorGraphNodeId;
use super::ColorGraphNodeFactory::ColorGraphNodeFactory;
use crate::ast::{GetNodeId, NodeId};
use crate::colors::color_registry::ColorRegistry;
use crate::colors::ColorId;
use crate::disambiguate::ColorGraphBuilder::EdgeReason;
use crate::graph::FixedPointGraphTraversal::{EdgeCallback, FixedPointGraphTraversal};
use crate::graph::GraphColoring::{GreedyGraphColoring, SubGraph};
use crate::utils::unwrap_as;
use crate::DefaultNameGenerator::DefaultNameGenerator;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use index::bit_set::GrowableBitSet;
use index::{newtype_index, vec::IndexVec};
use rustc_hash::{FxHashMap, FxHashSet};
use std::iter::FromIterator;
use swc_atoms::JsWord;

/**
 * Renames unrelated properties to the same name, using {@link Color}s provided by the typechecker.
 * This allows better compression as more properties can be given short names.
 *
 * <p>Properties are considered unrelated if they are never referenced from the same color or from a
 * subtype of each others' colors, thus this pass is only effective if type checking is enabled.
 *
 * <p>Example: <code>
 *   Foo.fooprop = 0;
 *   Foo.fooprop2 = 0;
 *   Bar.barprop = 0;
 * </code> becomes: <code>
 *   Foo.a = 0;
 *   Foo.b = 0;
 *   Bar.a = 0;
 * </code>
 */
pub struct AmbiguateProperties;

impl AmbiguateProperties {
    pub fn process(ast: &mut ast::Program, colours: &mut ColorRegistry) {
        let mut graphNodeFactory = ColorGraphNodeFactory::new(colours);

        let mut externedNames = FxHashSet::default();

        // Find all property references and record the types on which they occur.
        // Populate stringNodesToRename, propertyMap, quotedNames.
        let (mut properties, propertyMap, stringNodesToRename) =
            ProcessPropertiesAndConstructors::process(
                &mut graphNodeFactory,
                &mut externedNames,
                ast,
            );

        let types_to_add = graphNodeFactory.getAllKnownTypes();
        let mut graphBuilder = ColorGraphBuilder::new(&mut graphNodeFactory);
        graphBuilder.addAll(&types_to_add);
        let colorGraph = graphBuilder.build();
        for node in graphNodeFactory.getAllKnownTypes() {
            graphNodeFactory.nodes[node].subtypeIndices.insert(node); // Init subtyping as reflexive.
        }

        struct Traversal<'col, 'nf> {
            colorGraphNodeFactory: &'nf mut ColorGraphNodeFactory<'col>,
        }

        impl EdgeCallback<ColorGraphNodeId, EdgeReason> for Traversal<'_, '_> {
            fn traverseEdge(
                &mut self,
                subtype: &ColorGraphNodeId,
                e: &EdgeReason,
                supertype: &ColorGraphNodeId,
            ) -> bool {
                if subtype == supertype {
                    return false;
                }

                let (subtype, supertype) = self
                    .colorGraphNodeFactory
                    .nodes
                    .pick2_mut(*subtype, *supertype);
                // /**
                //  * Cheap path for when we're sure there's going to be a change.
                //  *
                //  * <p>Since bits only ever turn on, using more bits means there are definitely more
                //  * elements. This prevents of from needing to check cardinality or equality, which
                //  * would otherwise dominate the cost of computing the fixed point.
                //  *
                //  * <p>We're guaranteed to converge because the sizes will be euqal after the OR
                //  * operation.
                //  */
                // if self.colorGraphNodeFactory[subtype].subtypeIndices.size()
                //     > self.colorGraphNodeFactory[supertype].subtypeIndices.size()
                // {
                //     self.colorGraphNodeFactory[supertype]
                //         .subtypeIndices
                //         .or(self.colorGraphNodeFactory[subtype].subtypeIndices);
                //     return true;
                // }

                let startSize = supertype.subtypeIndices.count();
                supertype.subtypeIndices.union(&subtype.subtypeIndices);
                return supertype.subtypeIndices.count() > startSize;
            }
        }

        FixedPointGraphTraversal::newReverseTraversal(Traversal {
            colorGraphNodeFactory: &mut graphNodeFactory,
        })
        .computeFixedPoint(&colorGraph);

        // for (i, node) in graphNodeFactory.nodes.iter().enumerate() {
        //     println!("Color graph node {}:", i);
        //     println!("Associated with prop: {}", node.associatedProps.len() > 0);
        //     let color_id = node.color;
        //     println!("ColorId: {:?}", color_id);
        //     println!("Color: {:#?}", &graphNodeFactory.colours.colors[color_id]);

        //     println!("Nodes with color: ");
        //     for (node, &color) in graphNodeFactory.colours.node_to_color_map.iter() {
        //         if color == color_id {
        //             if let BoundNode::Ident(i) = node {
        //                 print!("Ident: '{:?}' ", i.sym);
        //             }
        //             println!("{:#?}", node);
        //         }
        //     }
        //     println!("======================");
        // }

        // dbg!(&graphNodeFactory.colours.disambiguationSupertypeGraph);

        // let dot = format!(
        //     "{:?}",
        //     petgraph::dot::Dot::with_config(&colorGraph, &[/*Config::NodeIndexLabel*/])
        // );

        // std::fs::write("colors.dot", dot).expect("Failed to output color graph");

        // Fill in all transitive edges in subtyping graph per property
        for &prop in propertyMap.values() {
            // TODO: bad clone():
            for color in properties[prop].relatedColorsSeeds.clone() {
                properties[prop]
                    .relatedColors
                    .union(&graphNodeFactory[color].subtypeIndices);
            }
            //   prop.relatedColorsSeeds = null;
        }

        let mut reservedNames = FxHashSet::from_iter(externedNames.iter().cloned());
        // .addAll(quotedNames);

        let mut numRenamedPropertyNames = 0;
        let mut numSkippedPropertyNames = 0;
        let mut nodes = Vec::with_capacity(propertyMap.len());
        for &prop in propertyMap.values() {
            if properties[prop].skipAmbiguating {
                numSkippedPropertyNames += 1;
                reservedNames.insert(properties[prop].oldName.clone());
            } else {
                numRenamedPropertyNames += 1;
                nodes.push(prop);
            }
        }

        let mut coloring = GreedyGraphColoring::new();
        let numNewPropertyNames = coloring.color::<_, _, PropertySubGraph, _>(
            // TODO: bad clone:
            nodes.clone(),
            |&a, &b| {
                /*
                 * Sorts Property objects by their count, breaking ties lexicographically to ensure a deterministic
                 * total ordering.
                 */
                let result = properties[a]
                    .numOccurrences
                    .cmp(&properties[b].numOccurrences);
                if result.is_eq() {
                    properties[a].oldName.cmp(&properties[b].oldName)
                } else {
                    result
                }
            },
            |&prop| properties[prop].numOccurrences,
            || PropertySubGraph::new(&properties),
        );

        // Generate new names for the properties that will be renamed.
        let mut nameGen = DefaultNameGenerator::new(reservedNames);
        let mut colorMap = IndexVec::with_capacity(numNewPropertyNames);
        for _ in 0..numNewPropertyNames {
            colorMap.push(nameGen.generateNextName());
        }

        // Translate the color of each Property instance to a name.
        for node in nodes {
            let color = coloring.get_color_of_node(&node);
            properties[node].newName = Some(colorMap[color].clone());
            //   renamingMap.put(node.getValue().oldName, node.getValue().newName);
        }

        // Actually assign the new names to the relevant STRING nodes in the AST.
        let mut renamer = UseSiteRenamer {
            stringNodesToRename,
            properties,
            propertyMap,
        };

        ast.visit_mut_with(&mut renamer);

        struct UseSiteRenamer {
            stringNodesToRename: FxHashSet<NodeId>,
            properties: IndexVec<PropertyId, Property>,
            propertyMap: FxHashMap<JsWord, PropertyId>,
        }

        // TODO: since there is a one-to-one relation between idents and use sites,
        // once we have renamed all id's we collected, we can abort the traversal
        impl VisitMut<'_> for UseSiteRenamer {
            // TODO:
            // We only rename idents in types for testing, to make writing tests easier.
            // In normal builds there's no point in renaming these idents, so we skip the work.
            // #[cfg(not(test))]
            // noop_visit_mut_type!();

            fn visit_mut_ident(&mut self, ident: &mut ast::Ident) {
                if self.stringNodesToRename.contains(&ident.node_id) {
                    let oldName = &ident.sym;
                    let p = self.propertyMap.get(oldName);
                    if let Some(&p) = self.propertyMap.get(oldName) {
                        let prop = &self.properties[p];
                        if let Some(new_name) = &prop.newName {
                            debug_assert!(oldName == &prop.oldName);
                            ident.sym = new_name.clone();
                        }
                    }
                }
            }
        }

        // // We may have renamed getter / setter properties.
        // // TODO(b/161947315): this shouldn't be the responsibility of AmbiguateProperties
        // GatherGetterAndSetterProperties.update(compiler, externs, root);

        // if (logger.isLoggable(Level.FINE)) {
        //   logger.fine("Collapsed " + numRenamedPropertyNames + " properties into "
        //               + numNewPropertyNames + " and skipped renaming "
        //               + numSkippedPropertyNames + " properties.");
        // }
    }
}

/**
 * A {@link SubGraph} that represents properties. The related types of
 * the properties are used to efficiently calculate adjacency information.
 */
struct PropertySubGraph<'a> {
    properties: &'a IndexVec<PropertyId, Property>,
    /** Types related to properties referenced in this subgraph. */
    relatedTypes: GrowableBitSet<ColorGraphNodeId>,
}

impl<'a> PropertySubGraph<'a> {
    pub fn new(properties: &'a IndexVec<PropertyId, Property>) -> Self {
        Self {
            properties,
            relatedTypes: GrowableBitSet::new_empty(),
        }
    }
}

impl SubGraph<PropertyId> for PropertySubGraph<'_> {
    /**
     * Returns true if prop is in an independent set from all properties in this
     * sub graph.  That is, if none of its related types intersects with the
     * related types for this sub graph.
     */
    fn isIndependentOf(&self, prop: &PropertyId) -> bool {
        !self
            .relatedTypes
            .intersects(&self.properties[*prop].relatedColors)
    }

    /**
     * Adds the node to the sub graph, adding all its related types to the
     * related types for the sub graph.
     */
    fn addNode(&mut self, prop: PropertyId) {
        self.relatedTypes
            .union(&self.properties[prop].relatedColors);
    }
}

/**
 * Finds all property references, recording the types on which they occur, and records all
 * constructors and their instance types in the {@link ColorGraphNodeFactory}.
 */
struct ProcessPropertiesAndConstructors<'col, 'a> {
    externedNames: &'a mut FxHashSet<JsWord>,
    stringNodesToRename: FxHashSet<NodeId>,
    properties: IndexVec<PropertyId, Property>,
    propertyMap: FxHashMap<JsWord, PropertyId>,
    colorGraphNodeFactory: &'a mut ColorGraphNodeFactory<'col>,
}

impl<'col, 'a> ProcessPropertiesAndConstructors<'col, 'a> {
    pub fn process(
        colorGraphNodeFactory: &'a mut ColorGraphNodeFactory<'col>,
        externedNames: &'a mut FxHashSet<JsWord>,
        ast: &ast::Program,
    ) -> (
        IndexVec<PropertyId, Property>,
        FxHashMap<JsWord, PropertyId>,
        FxHashSet<NodeId>,
    ) {
        let mut visitor = Self {
            externedNames,
            stringNodesToRename: Default::default(),
            properties: Default::default(),
            propertyMap: Default::default(),
            colorGraphNodeFactory,
        };

        ast.visit_with(&mut visitor);

        (
            visitor.properties,
            visitor.propertyMap,
            visitor.stringNodesToRename,
        )
    }

    /**
     * If a property node is eligible for renaming, stashes a reference to it and increments the
     * property name's access count.
     *
     * @param n The STRING node for a property
     */
    fn maybeMarkCandidate(&mut self, ident_node_id: NodeId, ident_sym: &JsWord, owner: ColorId) {
        if !self.externedNames.contains(ident_sym) {
            self.stringNodesToRename.insert(ident_node_id);
            self.recordProperty(ident_sym, owner);
        }
    }

    fn recordProperty(&mut self, name: &JsWord, color: ColorId) -> PropertyId {
        let prop = match self.propertyMap.get(name) {
            Some(prop) => *prop,
            None => {
                let prop = self.properties.push(Property::new(name.clone()));
                self.propertyMap.insert(name.clone(), prop);
                prop
            }
        };
        self.properties[prop].addRelatedColor(&mut self.colorGraphNodeFactory, color);
        prop
    }

    //   private void processObjectProperty(Node objectLit, Node key, Color type) {
    //     checkArgument(objectLit.isObjectLit() || objectLit.isObjectPattern(), objectLit);
    //     switch (key.getToken()) {
    //       case COMPUTED_PROP:
    //         if (key.getFirstChild().isStringLit()) {
    //           // If this quoted prop name is statically determinable, ensure we don't rename some
    //           // other property in a way that could conflict with it.
    //           //
    //           // This is largely because we store quoted member functions as computed properties and
    //           // want to be consistent with how other quoted properties invalidate property names.
    //           quotedNames.add(key.getFirstChild().getString());
    //         }
    //         break;
    //       case MEMBER_FUNCTION_DEF:
    //       case GETTER_DEF:
    //       case SETTER_DEF:
    //       case STRING_KEY:
    //         if (key.isQuotedString()) {
    //           // If this quoted prop name is statically determinable, ensure we don't rename some
    //           // other property in a way that could conflict with it
    //           quotedNames.add(key.getString());
    //         } else {
    //           maybeMarkCandidate(key, type);
    //         }
    //         break;

    //       case OBJECT_REST:
    //       case OBJECT_SPREAD:
    //         break; // Nothing to do.

    //       default:
    //         throw new IllegalStateException(
    //             "Unexpected child of " + objectLit.getToken() + ": " + key.toStringTree());
    //     }
    //   }

    fn handle_class(&mut self, class: &ast::Class, class_color: ColorId) {
        self.colorGraphNodeFactory.createNode(Some(class_color));
        // In theory all CLASS colors should be a function with a known prototype, but in
        // practice typecasts mean that this is not always the case.

        // ImmutableSet<Color> possiblePrototypes = classConstructorType.getPrototypes();
        // Color classPrototype =
        //     possiblePrototypes.isEmpty()
        //         ? StandardColors.UNKNOWN
        //         : Color.createUnion(possiblePrototypes);
        for member in &class.body {
            let prop = match member {
                ast::ClassMember::Method(m) => {
                    if let ast::PropName::Ident(i) = &m.key {
                        Some((i, m.is_static))
                    } else {
                        None
                    }
                }
                ast::ClassMember::PrivateMethod(m) => Some((&m.key.id, m.is_static)),
                ast::ClassMember::ClassProp(p) => {
                    if let ast::PropName::Ident(i) = &p.key {
                        Some((i, p.is_static))
                    } else {
                        None
                    }
                }
                ast::ClassMember::PrivateProp(p) => Some((&p.key.id, p.is_static)),
                ast::ClassMember::Constructor(_)
                | ast::ClassMember::TsIndexSignature(_)
                | ast::ClassMember::Empty(_) => None,
            };

            // TODO:
            // if (member.isQuotedString()) {
            //     // ignore get 'foo'() {} and prevent property name collisions
            //     // Note that only getters/setters are represented as quoted strings, not 'foo'() {}
            //     // see https://github.com/google/closure-compiler/issues/3071
            //     quotedNames.add(member.getString());
            //     continue;
            //   } else if (member.isComputedProp() || member.isComputedFieldDef()) {
            //     // ignore ['foo']() {}
            //     // for simple cases, we also prevent renaming collisions
            //     if (member.getFirstChild().isStringLit()) {
            //       quotedNames.add(member.getFirstChild().getString());
            //     }
            //     continue;
            //   }

            if let Some((id, is_static)) = prop {
                let owner = if is_static {
                    self.colorGraphNodeFactory.colours[class_color]
                        .staticType
                        .unwrap()
                } else {
                    class_color
                };
                self.maybeMarkCandidate(id.node_id, &id.sym, owner);
            }
        }
    }
}

impl Visit<'_> for ProcessPropertiesAndConstructors<'_, '_> {
    fn visit_member_expr(&mut self, node: &ast::MemberExpr) {
        node.visit_children_with(self);

        if node.computed {
            // TODO:
            //   // If this is a quoted property access (e.g. x['myprop']), we need to
            //   // ensure that we never rename some other property in a way that
            //   // could conflict with this quoted name.
            //   Node child = n.getLastChild();
            //   if (child.isStringLit()) {
            //     quotedNames.add(child.getString());
            //   }
        } else {
            // TODO: this is wrong (e.g. privatename). See notes.txt for info.
            let prop = unwrap_as!(node.prop.as_ref(), ast::Expr::Ident(i), i);
            let owner = self
                .colorGraphNodeFactory
                .colours
                .get_color_of_node(node.obj.node_id())
                .unwrap();
            self.maybeMarkCandidate(prop.node_id, &prop.sym, owner);

            // TODO:
            // if NodeUtil.isLhsOfAssign(node) || NodeUtil.isStatement(node.getParent()) {
            //     graphNodeFactory.createNode(type);
            //   }
        }
    }

    fn visit_call_expr(&mut self, node: &ast::CallExpr) {
        // TODO: objectDefineProperty
        // if isBindableObjectDefinePropertyCall(node.as_ref()) {
        //     node.type_args.visit_with(self);
        //     node.callee.visit_with(self);

        //     // Checked by isBindableObjectDefinePropertyCall
        //     assert!(node.args.len() == 3);
        //     // The obect the prop is defined on.
        //     node.args[0].visit_with(self);
        //     // Name of prop.
        //     node.args[1].visit_with(self);
        //     // The 3rd arg (the descriptor object) is handled below.

        //     todo!();
        //     //   Node typeObj = call.getSecondChild();
        //     //   Color type = getColor(typeObj);
        //     //   Node objectLiteral = typeObj.getNext();

        //     //   if (!objectLiteral.isObjectLit()) {
        //     //     return;
        //     //   }

        //     //   for (Node key = objectLiteral.getFirstChild(); key != null; key = key.getNext()) {
        //     //     processObjectProperty(objectLiteral, key, type);
        //     //   }
        // } else {
        node.visit_children_with(self);
        // }
    }

    fn visit_fn_decl(&mut self, node: &ast::FnDecl) {
        node.visit_children_with(self);

        // TODO:
        // handle ES5-style classes
        // graphNodeFactory.createNode(getColor(node.ident));
    }

    fn visit_fn_expr(&mut self, node: &ast::FnExpr) {
        node.visit_children_with(self);

        // TODO:
        // handle ES5-style classes
        // graphNodeFactory.createNode(getColor(node.ident));
    }

    fn visit_var_declarator(&mut self, node: &ast::VarDeclarator) {
        node.visit_children_with(self);

        // TODO:
        // handle ES5-style classes
        // if let ast::Pat::Ident(_) = &node.name {
        //     graphNodeFactory.createNode(getColor(node.name));
        // }
    }

    fn visit_object_lit(&mut self, node: &ast::ObjectLit) {
        node.visit_children_with(self);

        let owner = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(node.node_id)
            .unwrap();

        for prop in &node.props {
            // TODO: quoted props
            let name = match prop {
                ast::Prop::Shorthand(n) => Some((n.node_id, &n.sym)),
                ast::Prop::KeyValue(ast::KeyValueProp { key, .. })
                | ast::Prop::Getter(ast::GetterProp { key, .. })
                | ast::Prop::Setter(ast::SetterProp { key, .. })
                | ast::Prop::Method(ast::MethodProp { key, .. }) => match key {
                    ast::PropName::Ident(i) => Some((i.node_id, &i.sym)),
                    _ => None,
                },
                ast::Prop::Assign(_) => None,
                ast::Prop::Spread(_) => None,
            };
            if let Some((ident_node_id, ident_sym)) = name {
                self.maybeMarkCandidate(ident_node_id, ident_sym, owner);
            }
        }
    }

    fn visit_object_pat(&mut self, node: &ast::ObjectPat) {
        node.visit_children_with(self);
        todo!();
        //   // Object.defineProperties literals are handled at the CALL node, as we determine the type
        //   // differently than for regular object literals.
        //   if (objectLit.getParent().isCall()
        //       && NodeUtil.isObjectDefinePropertiesDefinition(objectLit.getParent())) {
        //     return;
        //   }

        //   // The children of an OBJECTLIT node are keys, where the values
        //   // are the children of the keys.
        //   Color type = getColor(objectLit);
        //   for (Node key = objectLit.getFirstChild(); key != null; key = key.getNext()) {
        //     processObjectProperty(objectLit, key, type);
        //   }
    }

    fn visit_class_decl(&mut self, node: &ast::ClassDecl) {
        node.visit_children_with(self);

        let class_color = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(node.node_id)
            .unwrap();

        self.handle_class(&node.class, class_color);
    }

    fn visit_class_expr(&mut self, node: &ast::ClassExpr) {
        node.visit_children_with(self);

        let class_color = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(node.node_id)
            .unwrap();

        self.handle_class(&node.class, class_color);
    }
}

/** Encapsulates the information needed for renaming a property. */
struct Property {
    oldName: JsWord,
    newName: Option<JsWord>,
    numOccurrences: usize,
    skipAmbiguating: bool,
    // All colors upon which this property was directly accessed. For "a.b" this includes "a"'s type
    relatedColorsSeeds: FxHashSet<ColorGraphNodeId>,
    // includes relatedTypesSeeds + all subtypes of those seed colors. For example if this property
    // was accessed off of Iterable, then this bitset will include Array as well.
    relatedColors: GrowableBitSet<ColorGraphNodeId>,
}

impl Property {
    pub fn new(name: JsWord) -> Self {
        Self {
            oldName: name,
            newName: None,
            numOccurrences: 0,
            skipAmbiguating: false,
            relatedColorsSeeds: Default::default(),
            relatedColors: GrowableBitSet::new_empty(),
        }
    }

    /** Marks this color as related to this property */
    pub fn addRelatedColor<'c>(
        &mut self,
        colorGraphNodeFactory: &mut ColorGraphNodeFactory<'c>,
        color_id: ColorId,
    ) {
        if self.skipAmbiguating {
            return;
        }

        self.numOccurrences += 1;

        let color = &colorGraphNodeFactory.colours[color_id];

        if color.isInvalidating
        /*|| color.getPropertiesKeepOriginalName()*/
        {
            self.skipAmbiguating = true;
            return;
        }

        let newColorGraphNode = colorGraphNodeFactory.createNode(Some(color_id));
        self.relatedColorsSeeds.insert(newColorGraphNode);
    }
}

newtype_index! {
    pub struct PropertyId {
        DEBUG_FORMAT = "PropertyId({})"
    }
}

#[cfg(test)]
mod tests {
    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, _| {
                let mut colours = crate::testing::get_colours(program.clone());
                super::AmbiguateProperties::process(&mut program, &mut colours);
                program
            },
            input,
            expected,
        );
    }
    fn test_same(input: &str) {
        test_transform(input, input);
    }

    // TODO: test ideas from disambiguation

    // TODO:
    // #[test]
    // fn testOneVar1() {
    //   test_transform("/** @constructor */ var Foo = function(){};Foo.prototype.b = 0;",
    //        "/** @constructor */ var Foo = function(){};Foo.prototype.a = 0;");
    // }

    // TODO:
    // #[test]
    // fn testOneVar2() {
    //   test_transform(lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "Foo.prototype = {b: 0};"),
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "Foo.prototype = {a: 0};"));
    // }

    // TODO:
    // #[test]
    // fn testOneVar3() {
    //   test_transform(
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "Foo.prototype = {get b() {return 0}};"),
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "Foo.prototype = {get a() {return 0}};"));
    // }

    // TODO:
    // #[test]
    // fn testOneVar4() {
    //   test_transform(
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "Foo.prototype = {set b(a) {}};"),
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "Foo.prototype = {set a(a) {}};"));
    // }

    // TODO:
    // #[test]
    // fn testTwoVar1() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.z=0;",
    //       "Foo.prototype.z=0;",
    //       "Foo.prototype.x=0;");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.a=0;",
    //       "Foo.prototype.a=0;",
    //       "Foo.prototype.b=0;");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testTwoVar2() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {z:0, z:1, x:0};");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {a:0, a:1, b:0};");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testTwoIndependentVar() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.b = 0;",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Bar.prototype.c = 0;");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.a=0;",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Bar.prototype.a=0;");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testTwoTypesTwoVar() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.r = 0;",
    //       "Foo.prototype.g = 0;",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Bar.prototype.c = 0;",
    //       "Bar.prototype.r = 0;");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.a=0;",
    //       "Foo.prototype.b=0;",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Bar.prototype.b=0;",
    //       "Bar.prototype.a=0;");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testUnion_withUnrelatedPropertyAccess() {
    //   String js =
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "/** @constructor */ var Bar = function(){};",
    //           "Foo.prototype.foodoo=0;",
    //           "Bar.prototype.bardoo=0;",
    //           // variable exists that could be a Foo or a Bar
    //           "/** @type {Foo|Bar} */",
    //           "var U = any();",
    //           // We don't actually access either foodoo or bardoo on it,
    //           // though, so it's OK if they end up having the same name
    //           "U.joint");
    //   String output =
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "/** @constructor */ var Bar = function(){};",
    //           "Foo.prototype.a=0;",
    //           "Bar.prototype.a=0;",
    //           "/** @type {Foo|Bar} */",
    //           "var U = any();",
    //           "U.b");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testUnion_withRelatedPropertyAccess() {
    //   String js =
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "/** @constructor */ var Bar = function(){};",
    //           "Foo.prototype.foodoo=0;",
    //           "Bar.prototype.bardoo=0;",
    //           // variable exists that could be a Foo or a Bar
    //           "/** @type {Foo|Bar} */",
    //           "var U = any();",
    //           // both foodoo and bardoo are accessed through that variable,
    //           // so they must have different names.
    //           "U.foodoo;",
    //           "U.bardoo");
    //   String output =
    //       lines(
    //           "/** @constructor */ var Foo = function(){};",
    //           "/** @constructor */ var Bar = function(){};",
    //           "Foo.prototype.b=0;",
    //           "Bar.prototype.a=0;",
    //           "/** @type {Foo|Bar} */",
    //           "var U = any();",
    //           "U.b;",
    //           "U.a");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testUnions() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "/** @constructor */ var Baz = function(){};",
    //       "/** @constructor */ var Bat = function(){};",
    //       "Foo.prototype.lone1=0;",
    //       "Bar.prototype.lone2=0;",
    //       "Baz.prototype.lone3=0;",
    //       "Bat.prototype.lone4=0;",
    //       "/** @type {Foo|Bar} */",
    //       "var U1 = any();",
    //       "U1.j1;",
    //       "U1.j2;",
    //       "/** @type {Baz|Bar} */",
    //       "var U2 = any();",
    //       "U2.j3;",
    //       "U2.j4;",
    //       "/** @type {Baz|Bat} */",
    //       "var U3 = any();",
    //       "U3.j5;",
    //       "U3.j6");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "/** @constructor */ var Baz = function(){};",
    //       "/** @constructor */ var Bat = function(){};",
    //       "Foo.prototype.c=0;",
    //       "Bar.prototype.e=0;",
    //       "Baz.prototype.e=0;",
    //       "Bat.prototype.c=0;",
    //       "/** @type {Foo|Bar} */",
    //       "var U1 = any();",
    //       "U1.a;",
    //       "U1.b;",
    //       "/** @type {Baz|Bar} */",
    //       "var U2 = any();",
    //       "U2.c;",
    //       "U2.d;",
    //       "/** @type {Baz|Bat} */",
    //       "var U3 = any();",
    //       "U3.a;",
    //       "U3.b");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testExtends() {
    //   this.enableClosurePass();

    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.x=0;",
    //       "/** @constructor \n @extends Foo */ var Bar = function(){};",
    //       "goog.inherits(Bar, Foo);",
    //       "Bar.prototype.y=0;",
    //       "Bar.prototype.z=0;",
    //       "/** @constructor */ var Baz = function(){};",
    //       "Baz.prototype.l=0;",
    //       "Baz.prototype.m=0;",
    //       "Baz.prototype.n=0;",
    //       "(new Baz).m\n");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.a=0;",
    //       "/** @constructor \n @extends Foo */ var Bar = function(){};",
    //       "goog.inherits(Bar, Foo);",
    //       "Bar.prototype.b=0;",
    //       "Bar.prototype.c=0;",
    //       "/** @constructor */ var Baz = function(){};",
    //       "Baz.prototype.b=0;",
    //       "Baz.prototype.a=0;",
    //       "Baz.prototype.c=0;",
    //       "(new Baz).a\n");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testLotsOfVars() {
    //   StringBuilder js = new StringBuilder();
    //   StringBuilder output = new StringBuilder();
    //   js.append("/** @constructor */ var Foo = function(){};\n");
    //   js.append("/** @constructor */ var Bar = function(){};\n");
    //   output.append(js);

    //   int vars = 10;
    //   for (int i = 0; i < vars; i++) {
    //     js.append("Foo.prototype.var").append(i).append(" = 0;");
    //     js.append("Bar.prototype.var").append(i + 10000).append(" = 0;");
    //     output.append("Foo.prototype.").append((char) ('a' + i)).append("=0;");
    //     output.append("Bar.prototype.").append((char) ('a' + i)).append("=0;");
    //   }
    //   test_transform(js.toString(), output.toString());
    // }

    // TODO:
    // #[test]
    // fn testLotsOfClasses() {
    //   StringBuilder b = new StringBuilder();
    //   int classes = 10;
    //   for (int i = 0; i < classes; i++) {
    //     String c = "Foo" + i;
    //     b.append("/** @constructor */ var ").append(c).append(" = function(){};\n");
    //     b.append(c).append(".prototype.varness").append(i).append(" = 0;");
    //   }
    //   String js = b.toString();
    //   test_transform(js, js.replaceAll("varness\\d+", "a"));
    // }

    // TODO:
    // #[test]
    // fn testFunctionType() {
    //   String js = lines(
    //       "/** @constructor */ function Foo(){};",
    //       "/** @return {Bar} */",
    //       "Foo.prototype.fun = function() { return new Bar(); };",
    //       "/** @constructor */ function Bar(){};",
    //       "Bar.prototype.bazz;",
    //       "(new Foo).fun().bazz();");
    //   String output = lines(
    //       "/** @constructor */ function Foo(){};",
    //       "/** @return {Bar} */",
    //       "Foo.prototype.a = function() { return new Bar(); };",
    //       "/** @constructor */ function Bar(){};",
    //       "Bar.prototype.a;",
    //       "(new Foo).a().a();");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testPrototypePropertiesAsObjLitKeys1() {
    //   test_transform("/** @constructor */ function Bar() {};" +
    //            "Bar.prototype = {2: function(){}, getA: function(){}};",
    //            "/** @constructor */ function Bar() {};" +
    //            "Bar.prototype = {2: function(){}, a: function(){}};");
    // }

    // TODO:
    // #[test]
    // fn testPrototypePropertiesAsObjLitKeys2() {
    //   test_same("/** @constructor */ function Bar() {};" +
    //            "Bar.prototype = {2: function(){}, 'getA': function(){}};");
    // }

    // TODO:
    // #[test]
    // fn testQuotedPrototypeProperty() {
    //   test_same("/** @constructor */ function Bar() {};" +
    //            "Bar.prototype['getA'] = function(){};" +
    //            "var bar = new Bar();" +
    //            "bar['getA']();");
    // }

    // TODO:
    // #[test]
    // fn testObjectDefineProperties() {
    //   String js =
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "Bar.prototype.bar = 0;",
    //           "/** @struct @constructor */ var Foo = function() {",
    //           "  this.bar_ = 'bar';",
    //           "};",
    //           "/** @type {?} */ Foo.prototype.bar;",
    //           "Object.defineProperties(Foo.prototype, {",
    //           "  bar: {",
    //           "    configurable: true,",
    //           "    enumerable: true,",
    //           "    /** @this {Foo} */ get: function() { return this.bar_;},",
    //           "    /** @this {Foo} */ set: function(value) { this.bar_ = value; }",
    //           "  }",
    //           "});");

    //   String result =
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "Bar.prototype.a = 0;",
    //           "/** @struct @constructor */ var Foo = function() {",
    //           "  this.b = 'bar';",
    //           "};",
    //           "/** @type {?} */ Foo.prototype.a;",
    //           "Object.defineProperties(Foo.prototype, {",
    //           "  a: {",
    //           "    configurable: true,",
    //           "    enumerable: true,",
    //           "    /** @this {Foo} */ get: function() { return this.b;},",
    //           "    /** @this {Foo} */ set: function(value) { this.b = value; }",
    //           "  }",
    //           "});");

    //   test_transform(js, result);
    // }

    // TODO:
    // #[test]
    // fn testObjectDefinePropertiesQuoted() {
    //   String js =
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "Bar.prototype.bar = 0;",
    //           "/** @struct @constructor */ var Foo = function() {",
    //           "  this.bar_ = 'bar';",
    //           "};",
    //           "/** @type {?} */ Foo.prototype['bar'];",
    //           "Object.defineProperties(Foo.prototype, {",
    //           "  'a': {",
    //           "    configurable: true,",
    //           "    enumerable: true,",
    //           "    /** @this {Foo} */ get: function() { return this.bar_;},",
    //           "    /** @this {Foo} */ set: function(value) { this.bar_ = value; }",
    //           "  }",
    //           "});");

    //   String result =
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "Bar.prototype.b = 0;",
    //           "/** @struct @constructor */ var Foo = function() {",
    //           "  this.b = 'bar';",
    //           "};",
    //           "/** @type {?} */ Foo.prototype['bar'];",
    //           "Object.defineProperties(Foo.prototype, {",
    //           "  'a': {",
    //           "    configurable: true,",
    //           "    enumerable: true,",
    //           "    /** @this {Foo} */ get: function() { return this.b;},",
    //           "    /** @this {Foo} */ set: function(value) { this.b = value; }",
    //           "  }",
    //           "});");

    //   test_transform(js, result);
    // }

    // TODO:
    // #[test]
    // fn testObjectDefinePropertiesComputed() {
    //   String js =
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "Bar.prototype.bar = 0;",
    //           "/** @struct @constructor */ var Foo = function() {",
    //           "  this.bar_ = 'bar';",
    //           "};",
    //           "/** @type {?} */ Foo.prototype['bar'];",
    //           "Object.defineProperties(Foo.prototype, {",
    //           "  ['a']: {",
    //           "    configurable: true,",
    //           "    enumerable: true,",
    //           "    /** @this {Foo} */ get: function() { return this.bar_;},",
    //           "    /** @this {Foo} */ set: function(value) { this.bar_ = value; }",
    //           "  }",
    //           "});");

    //   String result =
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "Bar.prototype.b = 0;",
    //           "/** @struct @constructor */ var Foo = function() {",
    //           "  this.b = 'bar';",
    //           "};",
    //           "/** @type {?} */ Foo.prototype['bar'];",
    //           "Object.defineProperties(Foo.prototype, {",
    //           "  ['a']: {",
    //           "    configurable: true,",
    //           "    enumerable: true,",
    //           "    /** @this {Foo} */ get: function() { return this.b;},",
    //           "    /** @this {Foo} */ set: function(value) { this.b = value; }",
    //           "  }",
    //           "});");

    //   test_transform(js, result);
    // }

    // TODO:
    // #[test]
    // fn testObjectDefinePropertiesMemberFn() {
    //   // NOTE: this is very odd code, and people should not be writing it anyway. We really just
    //   // don't want a crash.
    //   String js =
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "Bar.prototype.bar = 0;",
    //           "/** @struct @constructor */ var Foo = function() {",
    //           "  this.bar_ = 'bar';",
    //           "};",
    //           "/** @type {?} */ Foo.prototype['bar'];",
    //           "Object.defineProperties(Foo.prototype, {",
    //           "  a() {}",
    //           "});");

    //   String result =
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "Bar.prototype.a = 0;",
    //           "/** @struct @constructor */ var Foo = function() {",
    //           "  this.b = 'bar';",
    //           "};",
    //           "/** @type {?} */ Foo.prototype['bar'];",
    //           "Object.defineProperties(Foo.prototype, {",
    //           "  a() {}",
    //           "});");

    //   test_transform(js, result);
    // }

    // TODO:
    // #[test]
    // fn testOverlappingOriginalAndGeneratedNames() {
    //   test_transform(
    //       lines(
    //           "/** @constructor */ function Bar(){};",
    //           "Bar.prototype.b = function(){};",
    //           "Bar.prototype.a = function(){};",
    //           "var bar = new Bar();",
    //           "bar.b();"),
    //       lines(
    //           "/** @constructor */ function Bar(){};",
    //           "Bar.prototype.a = function(){};",
    //           "Bar.prototype.b = function(){};",
    //           "var bar = new Bar();",
    //           "bar.a();"));
    // }

    // TODO:
    // #[test]
    // fn testPropertyAddedToObject() {
    //   test_same("var foo = {}; foo.prop = '';");
    // }

    // TODO:
    // #[test]
    // fn testPropertyAddedToFunction() {
    //   test_same("var foo = function(){}; foo.prop = '';");
    // }

    // TODO:
    // #[test]
    // fn testPropertyAddedToFunctionIndirectly() {
    //   test_same(
    //       lines(
    //           "var foo = function(){}; foo.prop = ''; foo.baz = '';",
    //           "function f(/** function(): void */ fun) { fun.bar = ''; fun.baz = ''; }"));
    // }

    // TODO:
    // #[test]
    // fn testConstructorTreatedAsSubtypeOfFunction() {
    //   String js =
    //       lines(
    //           "Function.prototype.a = 1;", //
    //           "/** @constructor */",
    //           "function F() {}",
    //           "F.y = 2;");

    //   String output =
    //       lines(
    //           "Function.prototype.a = 1;", //
    //           // F is a subtype of Function, so we can't ambiguate "F.b" to "F.a".
    //           "/** @constructor */",
    //           "function F() {}",
    //           "F.b = 2;");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testPropertyOfObjectOfUnknownType() {
    //   test_same("var foo = x(); foo.prop = '';");
    // }

    // TODO:
    // #[test]
    // fn testPropertyOnParamOfUnknownType() {
    //   test_same(
    //       lines(
    //           "/** @constructor */ function Foo(){};",
    //            "Foo.prototype.prop = 0;",
    //            "function go(aFoo){",
    //            "  aFoo.prop = 1;",
    //            "}"));
    // }

    #[test]
    fn testSetPropertyOfGlobalThis() {
        test_transform("this.prop = 'bar'", "this.a = 'bar'");
    }

    // TODO:
    // #[test]
    // fn testReadPropertyOfGlobalThis() {
    //   test_same(externs(EXTERNS + "Object.prototype.prop;"), srcs("f(this.prop);"));
    // }

    #[test]
    fn testSetQuotedPropertyOfThis() {
        test_same("this['prop'] = 'bar';");
    }

    // TODO:
    // #[test]
    // fn testExternedPropertyName() {
    //   test_transform(
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "/** @override */ Bar.prototype.toString = function(){};",
    //           "Bar.prototype.func = function(){};",
    //           "var bar = new Bar();",
    //           "bar.toString();"),
    //       lines(
    //           "/** @constructor */ var Bar = function(){};",
    //           "/** @override */ Bar.prototype.toString = function(){};",
    //           "Bar.prototype.a = function(){};",
    //           "var bar = new Bar();",
    //           "bar.toString();"));
    // }

    // TODO:
    // #[test]
    // fn testExternedPropertyNameDefinedByObjectLiteral() {
    //   test_same("/**@constructor*/function Bar(){};Bar.prototype.factory");
    // }

    // TODO:
    // #[test]
    // fn testStaticAndInstanceMethodWithSameName() {
    //   test_transform("/** @constructor */function Bar(){}; Bar.getA = function(){}; " +
    //        "Bar.prototype.getA = function(){}; Bar.getA();" +
    //        "var bar = new Bar(); bar.getA();",
    //        "/** @constructor */function Bar(){}; Bar.a = function(){};" +
    //        "Bar.prototype.a = function(){}; Bar.a();" +
    //        "var bar = new Bar(); bar.a();");
    // }

    // TODO:
    // #[test]
    // fn testStaticAndInstanceProperties() {
    //   test_transform("/** @constructor */function Bar(){};" +
    //        "Bar.getA = function(){}; " +
    //        "Bar.prototype.getB = function(){};",
    //        "/** @constructor */function Bar(){}; Bar.a = function(){};" +
    //        "Bar.prototype.a = function(){};");
    // }

    // TODO:
    // #[test]
    // fn testStaticAndSubInstanceProperties() {
    //   this.enableClosurePass();

    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.x=0;",
    //       "/** @constructor \n @extends Foo */ var Bar = function(){};",
    //       "goog.inherits(Bar, Foo);",
    //       "Bar.y=0;",
    //       "Bar.prototype.z=0;\n");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.a=0;",
    //       "/** @constructor \n @extends Foo */ var Bar = function(){};",
    //       "goog.inherits(Bar, Foo);",
    //       "Bar.a=0;",
    //       "Bar.prototype.a=0;\n");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testStatic() {
    //   String js = lines(
    //     "/** @constructor */ var Foo = function() {};",
    //     "Foo.x = 0;",
    //     "/** @constructor */ var Bar = function() {};",
    //     "Bar.y = 0;");

    //   String output = lines(
    //     "/** @constructor */ var Foo = function() {};",
    //     "Foo.a = 0;",
    //     "/** @constructor */ var Bar = function() {};",
    //     "Bar.a = 0;");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testClassWithStaticsPassedToUnrelatedFunction() {
    //   String js = lines(
    //     "/** @constructor */ var Foo = function() {};",
    //     "Foo.x = 0;",
    //     "/** @param {!Function} x */ function f(x) { x.y = 1; x.z = 2; }",
    //     "f(Foo)");
    //   String output = lines(
    //     "/** @constructor */ var Foo = function() {};",
    //     "Foo.a = 0;",
    //     "/** @param {!Function} x */ function f(x) { x.y = 1; x.z = 2; }",
    //     "f(Foo)");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testClassWithStaticsPassedToRelatedFunction() {
    //   String js = lines(
    //     "/** @constructor */ var Foo = function() {};",
    //     "Foo.x = 0;",
    //     "/** @param {!Function} x */ function f(x) { x.y = 1; x.x = 2;}",
    //     "f(Foo)");
    //   test_same(js);
    // }

    // TODO:
    // #[test]
    // fn testFunctionInstanceProtoype_isInvalidating() {
    //   String js =
    //       lines(
    //           "class Foo {",
    //           "  bar() { }",
    //           "}",
    //           "",
    //           "function addPrototypeProp(/** !Function */ ctor) {",
    //           "  ctor.prototype.kip = 0;",
    //           "}");
    //   String output =
    //       lines(
    //           "class Foo {",
    //           "  a() { }",
    //           "}",
    //           "",
    //           "function addPrototypeProp(/** !Function */ ctor) {",
    //           "  ctor.prototype.kip = 0;",
    //           "}");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testTypeMismatch() {
    //   ignoreWarnings(DiagnosticGroups.CHECK_TYPES);
    //   test_same(lines(
    //       "/** @constructor */var Foo = function(){};",
    //       "/** @constructor */var Bar = function(){};",
    //       "Foo.prototype.b = 0;",
    //       "/** @type {Foo} */",
    //       "var F = new Bar();"));
    // }

    // TODO:
    // #[test]
    // fn testRenamingMap() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.z=0;",
    //       "Foo.prototype.z=0;",
    //       "Foo.prototype.x=0;",
    //       "Foo.prototype.y=0;");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype.a=0;",
    //       "Foo.prototype.a=0;",
    //       "Foo.prototype.b=0;",
    //       "Foo.prototype.c=0;");
    //   test_transform(js, output);

    //   Map<String, String> answerMap = new HashMap<>();
    //   answerMap.put("x", "b");
    //   answerMap.put("y", "c");
    //   answerMap.put("z", "a");
    //   assertThat(lastPass.getRenamingMap()).isEqualTo(answerMap);
    // }

    // TODO:
    // #[test]
    // fn testInline() {
    //   String js = lines(
    //       "/** @interface */ function Foo(){}",
    //       "Foo.prototype.x = function(){};",
    //       "/**",
    //       " * @constructor",
    //       " * @implements {Foo}",
    //       " */",
    //       "function Bar(){}",
    //       "Bar.prototype.y;",
    //       "/** @override */",
    //       "Bar.prototype.x = function() { return this.y; };",
    //       "Bar.prototype.z = function() {};\n"
    //       // Simulates inline getters.
    //       + "/** @type {Foo} */ (new Bar).y;");
    //   String output = lines(
    //       "/** @interface */ function Foo(){}",
    //       "Foo.prototype.b = function(){};",
    //       "/**",
    //       " * @constructor",
    //       " * @implements {Foo}",
    //       " */",
    //       "function Bar(){}",
    //       "Bar.prototype.a;",
    //       "/** @override */",
    //       "Bar.prototype.b = function() { return this.a; };",
    //       "Bar.prototype.c = function() {};\n"
    //       // Simulates inline getters.
    //       + "(new Bar).a;");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testImplementsAndExtends() {
    //   String js = lines(
    //       "/** @interface */ function Foo() {}",
    //       "/** @constructor */ function Bar(){}",
    //       "Bar.prototype.y = function() { return 3; };",
    //       "/**",
    //       " * @constructor",
    //       " * @extends {Bar}",
    //       " * @implements {Foo}",
    //       " */",
    //       "function SubBar(){ }",
    //       "/** @param {Foo} x */ function f(x) { x.z = 3; }",
    //       "/** @param {SubBar} x */ function g(x) { x.z = 3; }");
    //   String output = lines(
    //       "/** @interface */ function Foo(){}",
    //       "/** @constructor */ function Bar(){}",
    //       "Bar.prototype.b = function() { return 3; };",
    //       "/**",
    //       " * @constructor",
    //       " * @extends {Bar}",
    //       " * @implements {Foo}",
    //       " */",
    //       "function SubBar(){}",
    //       "/** @param {Foo} x */ function f(x) { x.a = 3; }",
    //       "/** @param {SubBar} x */ function g(x) { x.a = 3; }");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testImplementsAndExtends_respectsUndeclaredProperties() {
    //   String js =
    //       lines(
    //           "/** @interface */ function A() {}",
    //           "/**",
    //           " * @constructor",
    //           " */",
    //           "function C1(){}",
    //           "/**",
    //           " * @constructor",
    //           " * @extends {C1}",
    //           " * @implements {A}",
    //           " */",
    //           "function C2(){}",
    //           "/** @param {C1} x */ function f(x) { x.y = 3; }",
    //           "/** @param {A} x */ function g(x) { x.z = 3; }");
    //   String output =
    //       lines(
    //           "/** @interface */ function A(){}",
    //           "/**",
    //           " * @constructor",
    //           " */",
    //           "function C1(){}",
    //           "/**",
    //           " * @constructor",
    //           " * @extends {C1}",
    //           " * @implements {A}",
    //           " */",
    //           "function C2(){}",
    //           "/** @param {C1} x */ function f(x) { x.a = 3; }",
    //           "/** @param {A} x */ function g(x) { x.b = 3; }");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testImplementsAndExtendsEs6Class_respectsUndeclaredProperties() {
    //   String js =
    //       lines(
    //           "/** @interface */",
    //           "class A {",
    //           "  constructor() {",
    //           // Optional property; C2 does not need to declare it implemented.
    //           "    /** @type {number|undefined} */",
    //           "    this.y;",
    //           "  }",
    //           "}",
    //           "class C1 {",
    //           "  constructor() {",
    //           "    /** @type {number} */",
    //           "    this.z;",
    //           "  }",
    //           "}",
    //           "/** @implements {A} */",
    //           "class C2 extends C1 {}",
    //           "/** @param {A} x */ function f(x) { x.y = 3; }",
    //           "/** @param {C1} x */ function g(x) { x.z = 3; }");
    //   String output =
    //       lines(
    //           "/** @interface */",
    //           "class A {",
    //           "  constructor() {",
    //           "    /** @type {number|undefined} */",
    //           "    this.a;",
    //           "  }",
    //           "}",
    //           "class C1 {",
    //           "  constructor() {",
    //           "    /** @type {number} */",
    //           "    this.b;",
    //           "  }",
    //           "}",
    //           "/** @implements {A} */",
    //           "class C2 extends C1 {}",
    //           "/** @param {A} x */ function f(x) { x.a = 3; }",
    //           "/** @param {C1} x */ function g(x) { x.b = 3; }");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testExtendsInterface() {
    //   String js = lines(
    //       "/** @interface */ function A() {}",
    //       "/** @interface \n @extends {A} */ function B() {}",
    //       "/** @param {A} x */ function f(x) { x.y = 3; }",
    //       "/** @param {B} x */ function g(x) { x.z = 3; }\n");
    //   String output = lines(
    //       "/** @interface */ function A(){}",
    //       "/** @interface \n @extends {A} */ function B(){}",
    //       "/** @param {A} x */ function f(x) { x.a = 3; }",
    //       "/** @param {B} x */ function g(x) { x.b = 3; }\n");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testInterfaceWithSubInterfaceAndDirectImplementors() {
    //   ignoreWarnings(DiagnosticGroups.CHECK_TYPES);
    //   test_transform(
    //       lines(
    //           "/** @interface */ function Foo(){};",
    //           "Foo.prototype.foo = function(){};",
    //           "/** @interface @extends {Foo} */ function Bar(){};",
    //           "/** @constructor @implements {Foo} */ function Baz(){};",
    //           "Baz.prototype.baz = function(){};"),
    //       lines(
    //           "/** @interface */ function Foo(){};",
    //           "Foo.prototype.b = function(){};",
    //           "/** @interface @extends {Foo} */ function Bar(){};",
    //           "/** @constructor @implements {Foo} */ function Baz(){};",
    //           "Baz.prototype.a = function(){};"));
    // }

    // TODO:
    // #[test]
    // fn testPredeclaredType() {
    //   this.enableClosurePass();
    //   this.enableRewriteClosureProvides();

    //   String js =
    //       lines(
    //           "goog.forwardDeclare('goog.Foo');",
    //           "/** @constructor */ ",
    //           "function A() {",
    //           "  this.x = 3;",
    //           "}",
    //           "/** @param {goog.Foo} x */",
    //           "function f(x) { x.y = 4; }");
    //   String result =
    //       lines(
    //           "/** @constructor */ ",
    //           "function A() {",
    //           "  this.a = 3;",
    //           "}",
    //           "/** @param {goog.Foo} x */",
    //           "function f(x) { x.y = 4; }");
    //   test_transform(
    //       externs(EXTERNS + new TestExternsBuilder().addClosureExterns().build()),
    //       srcs(js),
    //       expected(result));
    // }

    // TODO:
    // #[test]
    // fn testBug14291280() {
    //   String js = lines(
    //       "/** @constructor \n @template T */\n",
    //       "function C() {\n",
    //       "  this.aa = 1;\n",
    //       "}\n",
    //       "/** @return {C.<T>} */\n",
    //       "C.prototype.method = function() {\n",
    //       "  return this;\n",
    //       "}\n",
    //       "/** @type {C.<string>} */\n",
    //       "var x = new C().method();\n",
    //       "x.bb = 2;\n",
    //       "x.cc = 3;");
    //   String result = lines(
    //       "/** @constructor \n @template T */\n",
    //       "function C() {",
    //       "  this.b = 1;",
    //       "}",
    //       "/** @return {C.<T>} */\n",
    //       "C.prototype.a = function() {",
    //       "  return this;",
    //       "};",
    //       "/** @type {C.<string>} */\n",
    //       "var x = (new C).a();",
    //       "x.c = 2;",
    //       "x.d = 3;");
    //   test_transform(js, result);
    // }

    // TODO:
    // #[test]
    // fn testAmbiguateWithAnAlias() {
    //   String js = lines(
    //       "/** @constructor */ function Foo() { this.abc = 5; }\n",
    //       "/** @const */ var alias = Foo;\n",
    //       "/** @constructor @extends alias */\n",
    //       "function Bar() {\n",
    //       "  this.xyz = 7;\n",
    //       "}");
    //   String result = lines(
    //       "/** @constructor */ function Foo() { this.a = 5; }\n",
    //       "/** @const */ var alias = Foo;\n",
    //       "/** @constructor @extends alias */\n",
    //       "function Bar() {\n",
    //       "  this.b = 7;\n",
    //       "}");
    //   test_transform(js, result);
    // }

    // TODO:
    // #[test]
    // fn testAmbiguateWithAliases() {
    //   String js = lines(
    //       "/** @constructor */ function Foo() { this.abc = 5; }\n",
    //       "/** @const */ var alias = Foo;\n",
    //       "/** @constructor @extends alias */\n",
    //       "function Bar() {\n",
    //       "  this.def = 7;\n",
    //       "}\n",
    //       "/** @constructor @extends alias */\n",
    //       "function Baz() {\n",
    //       "  this.xyz = 8;\n",
    //       "}");
    //   String result = lines(
    //       "/** @constructor */ function Foo() { this.a = 5; }\n",
    //       "/** @const */ var alias = Foo;\n",
    //       "/** @constructor @extends alias */\n",
    //       "function Bar() {\n",
    //       "  this.b = 7;\n",
    //       "}\n",
    //       "/** @constructor @extends alias */\n",
    //       "function Baz() {\n",
    //       "  this.b = 8;\n",
    //       "}");
    //   test_transform(js, result);
    // }

    // // See https://github.com/google/closure-compiler/issues/1358
    // TODO:
    // #[test]
    // fn testAmbiguateWithStructuralInterfaces() {
    //   String js = lines(
    //       "/** @record */",
    //       "function Record() {}",
    //       "/** @type {number|undefined} */",
    //       "Record.prototype.recordProp;",
    //       "",
    //       "function f(/** !Record */ a) { use(a.recordProp); }",
    //       "",
    //       "/** @constructor */",
    //       "function Type() {",
    //       "  /** @const */",
    //       "  this.classProp = 'a';",
    //       "}",
    //       "f(new Type)");

    //   String expected =
    //       lines(
    //           "/** @record */",
    //           "function Record() {}",
    //           "/** @type {number|undefined} */",
    //           "Record.prototype.recordProp;",
    //           "",
    //           "function f(/** !Record */ a) { use(a.recordProp); }",
    //           "",
    //           "/** @constructor */",
    //           "function Type() {",
    //           "  /** @const */",
    //           "  this.a = 'a';",
    //           "}",
    //           "f(new Type)");

    //   test_transform(js, expected);
    // }

    // TODO:
    // #[test]
    // fn structuralTypesNotAmbiguated_forwardRefsNotInUnion() {
    //   // edge case where the early referenced to "Params" caused data.name => data.a
    //   test_transform(
    //       lines(
    //           "class Foo {",
    //           "  method() {}",
    //           "}",
    //           "/** @param {!Params=} data */",
    //           "function f(data) {",
    //           "  return data.name;",
    //           "}",
    //           "/** @typedef {{name: string}} */",
    //           "let Params;"),
    //       lines(
    //           "class Foo {",
    //           "  a() {}",
    //           "}",
    //           "/** @param {!Params=} data */",
    //           "function f(data) {",
    //           "  return data.name;",
    //           "}",
    //           "/** @typedef {{name: string}} */",
    //           "let Params;"));
    // }

    // TODO:
    // #[test]
    // fn testObjectSpreadDoesNotCrash() {
    //   test_same("var lit1 = {...a};");
    // }

    // TODO:
    // #[test]
    // fn testObjectRestDoesNotCrash() {
    //   test_same("var {...a} = b;");
    // }

    // // See https://github.com/google/closure-compiler/issues/2119
    // TODO:
    // #[test]
    // fn testUnrelatedObjectLiterals() {
    //   test_same(lines(
    //       "/** @constructor */ function Foo() {}",
    //       "/** @constructor */ function Bar() {}",
    //       "var lit1 = {a: new Foo()};",
    //       "var lit2 = {b: new Bar()};"));
    // }

    // TODO:
    // #[test]
    // fn testObjectLitTwoVar() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {z:0, z:1, x:0};");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {a:0, a:1, b:0};");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testObjectLitTwoIndependentVar() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {b: 0};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Bar.prototype = {c: 0};");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {a: 0};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Bar.prototype = {a: 0};");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testObjectLitTwoTypesTwoVar() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {r: 0, g: 0};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Bar.prototype = {c: 0, r: 0};");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {a: 0, b: 0};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Bar.prototype = {b: 0, a: 0};");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testObjectLitUnion() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Foo.prototype = {foodoo: 0};",
    //       "Bar.prototype = {bardoo: 0};",
    //       "/** @type {Foo|Bar} */",
    //       "var U = any();",
    //       "U.joint;");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "Foo.prototype = {a: 0};",
    //       "Bar.prototype = {a: 0};",
    //       "/** @type {Foo|Bar} */",
    //       "var U = any();",
    //       "U.b;");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testObjectLitUnions() {
    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "/** @constructor */ var Baz = function(){};",
    //       "/** @constructor */ var Bat = function(){};",
    //       "Foo.prototype = {lone1: 0};",
    //       "Bar.prototype = {lone2: 0};",
    //       "Baz.prototype = {lone3: 0};",
    //       "Bat.prototype = {lone4: 0};",
    //       "/** @type {Foo|Bar} */",
    //       "var U1 = any();",
    //       "U1.j1;",
    //       "U1.j2;",
    //       "/** @type {Baz|Bar} */",
    //       "var U2 = any();",
    //       "U2.j3;",
    //       "U2.j4;",
    //       "/** @type {Baz|Bat} */",
    //       "var U3 = any();",
    //       "U3.j5;",
    //       "U3.j6");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "/** @constructor */ var Bar = function(){};",
    //       "/** @constructor */ var Baz = function(){};",
    //       "/** @constructor */ var Bat = function(){};",
    //       "Foo.prototype = {c: 0};",
    //       "Bar.prototype = {e: 0};",
    //       "Baz.prototype = {e: 0};",
    //       "Bat.prototype = {c: 0};",
    //       "/** @type {Foo|Bar} */",
    //       "var U1 = any();",
    //       "U1.a;",
    //       "U1.b;",
    //       "/** @type {Baz|Bar} */",
    //       "var U2 = any();",
    //       "U2.c;",
    //       "U2.d;",
    //       "/** @type {Baz|Bat} */",
    //       "var U3 = any();",
    //       "U3.a;",
    //       "U3.b");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testObjectLitExtends() {
    //   this.enableClosurePass();

    //   String js = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {x: 0};",
    //       "/** @constructor \n @extends Foo */ var Bar = function(){};",
    //       "goog.inherits(Bar, Foo);",
    //       "Bar.prototype = {y: 0, z: 0};",
    //       "/** @constructor */ var Baz = function(){};",
    //       "Baz.prototype = {l: 0, m: 0, n: 0};",
    //       "(new Baz).m\n");
    //   String output = lines(
    //       "/** @constructor */ var Foo = function(){};",
    //       "Foo.prototype = {a: 0};",
    //       "/** @constructor \n @extends Foo */ var Bar = function(){};",
    //       "goog.inherits(Bar, Foo);",
    //       "Bar.prototype = {b: 0, c: 0};",
    //       "/** @constructor */ var Baz = function(){};",
    //       "Baz.prototype = {b: 0, a: 0, c: 0};",
    //       "(new Baz).a\n");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testObjectLitLotsOfClasses() {
    //   StringBuilder b = new StringBuilder();
    //   int classes = 10;
    //   for (int i = 0; i < classes; i++) {
    //     String c = "Foo" + i;
    //     b.append("/** @constructor */ var ").append(c).append(" = function(){};\n");
    //     b.append(c).append(".prototype = {varness").append(i).append(": 0};");
    //   }
    //   String js = b.toString();
    //   test_transform(js, js.replaceAll("varness\\d+", "a"));
    // }

    // TODO:
    // #[test]
    // fn testObjectLitFunctionType() {
    //   String js = lines(
    //       "/** @constructor */ function Foo(){};",
    //       "Foo.prototype = { /** @return {Bar} */ fun: function() { return new Bar(); } };",
    //       "/** @constructor */ function Bar(){};",
    //       "Bar.prototype.bazz;",
    //       "(new Foo).fun().bazz();");
    //   String output = lines(
    //       "/** @constructor */ function Foo(){};",
    //       "Foo.prototype = { /** @return {Bar} */ a: function() { return new Bar(); } };",
    //       "/** @constructor */ function Bar(){};",
    //       "Bar.prototype.a;",
    //       "(new Foo).a().a();");
    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testObjectLitOverlappingOriginalAndGeneratedNames() {
    //   test_transform(
    //       lines(
    //           "/** @constructor */ function Bar(){};",
    //           "Bar.prototype = {b: function(){}, a: function(){}};",
    //           "var bar = new Bar();",
    //           "bar.b();"),
    //       lines(
    //           "/** @constructor */ function Bar(){};",
    //           "Bar.prototype = {a: function(){}, b: function(){}};",
    //           "var bar = new Bar();",
    //           "bar.a();"));
    // }

    // TODO:
    // #[test]
    // fn testEnum() {
    //   test_same(
    //       lines(
    //           "/** @enum {string} */ var Foo = {X: 'y'};",
    //           "var x = Foo.X"));
    // }

    // TODO:
    // #[test]
    // fn testUnannotatedConstructorsDontCrash() {
    //   test_same(lines(
    //       "function Foo() {}",
    //       "Foo.prototype.a;",
    //       "function Bar() {}",
    //       "Bar.prototype.a;"));
    // }

    // TODO:
    // #[test]
    // fn testGenericPrototypeObject() {
    //   String js = lines(
    //       "/**",
    //       " * @constructor",
    //       " * @template T",
    //       " */",
    //       "function Foo() {",
    //       "  this.a = 1;",
    //       "}",
    //       "/** @constructor @extends {Foo<number>} */",
    //       "function Bar() {}",
    //       "/** @constructor */",
    //       "function Baz() {",
    //       "  this.b = 2;",
    //       "}");

    //   String output = lines(
    //       "/**",
    //       " * @constructor",
    //       " * @template T",
    //       " */",
    //       "function Foo() {",
    //       "  this.a = 1;",
    //       "}",
    //       "/** @constructor @extends {Foo<number>} */",
    //       "function Bar() {}",
    //       "/** @constructor */",
    //       "function Baz() {",
    //       "  this.a = 2;",
    //       "}");

    //   test_transform(js, output);
    // }

    #[test]
    fn testRelatedInstanceAndPrototypePropNotAmbiguated() {
        test_transform(
            "
class Foo {
    constructor() {
        this.y = 0;
    }
    x() {}
}",
            "
class Foo {
    constructor() {
        this.b = 0;
    }
    a() {}
}",
        );
    }

    #[test]
    fn testRelatedInstanceAndSuperclassPrototypePropNotAmbiguated() {
        test_transform(
            "
class Foo {
    x() {}
}
class Bar extends Foo {
    constructor() {
        super();
        this.y = 0;
    }
}",
            "
class Foo {
    a() {}
}
class Bar extends Foo {
    constructor() {
        super();
        this.b = 0;
    }
}",
        );
    }

    // TODO:
    // #[test]
    // fn testPropertiesWithTypesThatHaveBeenNarrowed() {
    //   String js = lines(
    //       "/** @constructor */",
    //       "function Foo() {",
    //       "  /** @type {?Object} */",
    //       "  this.prop1 = {};",
    //       "  /** @type {?Object} */",
    //       "  this.prop2;",
    //       "  this.headerObserver_;",
    //       "}",
    //       "Foo.prototype.m = function() {",
    //       "  this.prop2 = {};",
    //       "  return this.headerObserver_;",
    //       "};");

    //   String output = lines(
    //       "/** @constructor */",
    //       "function Foo() {",
    //       "  /** @type {?Object} */",
    //       "  this.d = {};",
    //       "  /** @type {?Object} */",
    //       "  this.b;",
    //       "  this.a;",
    //       "}",
    //       "Foo.prototype.c = function() {",
    //       "  this.b = {};",
    //       "  return this.a;",
    //       "};");

    //   test_transform(js, output);
    // }

    // TODO:
    // #[test]
    // fn testDontRenamePrototypeWithoutExterns() {
    //   String js = lines(
    //     "/** @interface */",
    //     "function Foo() {}",
    //     "/** @return {!Foo} */",
    //     "Foo.prototype.foo = function() {};");

    //   String output = lines(
    //     "/** @interface */",
    //     "function Foo() {}",
    //     "/** @return {!Foo} */",
    //     "Foo.prototype.a = function() {};");

    //   test_transform(externs(""), srcs(js), expected(output));
    // }

    // TODO:
    // #[test]
    // fn testInvalidRenameFunction_doesNotCrash() {
    //   test_same("const p = JSCompiler_renameProperty('foo', 0, 1)");
    //   test_same("const p = JSCompiler_renameProperty(0)");
    //   test_same("const p = JSCompiler_renameProperty('a.b')");
    // }

    // TODO:
    // #[test]
    // fn testJSCompiler_renameProperty_twoArgs_blocksAmbiguation() {
    //   test_same(
    //       lines(
    //           "/** @constructor */",
    //           "function Foo() {}",
    //           "Foo.prototype.bar = 3;",
    //           "const barName = JSCompiler_renameProperty('bar', Foo);"));
    // }

    // TODO:
    // #[test]
    // fn testJSCompiler_renameProperty_oneArg_blocksAmbiguation() {
    //   test_same(
    //       lines(
    //           "/** @constructor */",
    //           "function Foo() {}",
    //           "Foo.prototype.bar = 3;",
    //           "const barName = JSCompiler_renameProperty('bar');"));
    // }

    #[test]
    fn testSingleClass_withSingleMemberFn_ambiguated() {
        test_transform("class Foo { methodFoo() {} }", "class Foo { a() {} }");
    }

    #[test]
    fn testSingleClass_withSingleMemberField_ambiguated() {
        test_transform("class Foo { field = 2; } ", "class Foo { a = 2; }");
    }

    #[test]
    fn testSingleClass_withSingleQuotedMemberField_notAmbiguated() {
        test_same("class Foo { 'field' = 2; }");
    }

    #[test]
    fn testSingleClass_withSingleStaticMemberField_ambiguated() {
        test_transform(
            "class Foo { static field = 2; }",
            "class Foo { static a = 2; }",
        );
    }

    #[test]
    fn testSingleClass_withSingleComputedMemberField_notAmbiguated() {
        test_same("class Foo { ['field'] = 2; }");
    }

    #[test]
    fn testSingleClass_withSingleStaticComputedMemberField_notAmbiguated() {
        test_same("class Foo { static ['field'] = 2; }");
    }

    #[test]
    fn testSingleClass_withSingleNumberMemberField_notAmbiguated() {
        test_same("class Foo { 1 = 2; }");
    }

    #[test]
    fn testSingleClass_withSingleStaticNumberMemberField_notAmbiguated() {
        test_same("class Foo { static 1 = 2; }");
    }

    #[test]
    fn testQuotedMemberFieldInClass_notAmbiguated() {
        test_same("class Foo { 'fieldFoo' }");
    }

    // TODO:
    // #[test]
    // fn testQuotedMemberFieldInClassReservesPropertyName() {
    //   test_transform(
    //       "/** @unrestricted */ class Foo { 'a'; foo; }",
    //       "/** @unrestricted */ class Foo { 'a'; b; }");
    // }

    #[test]
    fn testSingleClass_withTwoMemberFields_notAmbiguated() {
        test_transform("class Foo { field1; field2 = 2;}", "class Foo { a; b = 2;}");
    }

    #[test]
    fn testSingleClass_withStaticAndPrototypeMemberFields_ambiguated() {
        test_transform(
            "class Foo { static staticField; memberField; }",
            "class Foo { static a; a; }",
        );
    }

    #[test]
    fn testTwoUnrelatedClasses_withMemberFields_ambiguated() {
        test_transform(
            "class Foo { fieldFoo; } class Bar { fieldBar; }",
            "class Foo { a; } class Bar { a; }",
        );
    }

    #[test]
    fn testTwoUnrelatedClasses_withStaticMemberFields_ambiguated() {
        test_transform(
            "
class Foo { static fieldFoo = 2; }
class Bar { static fieldBar = 2; }",
            "
class Foo { static a = 2; }
class Bar { static a = 2; }",
        );
    }

    #[test]
    fn testEs6SuperclassStaticField_notAmbiguated() {
        test_transform(
            "
class Foo { static fieldFoo; }
class Bar extends Foo { static fieldBar; }",
            // Since someone could access Foo.methodFoo() through Bar, make sure the fields get
            // distinct names.
            "
class Foo { static b; }
class Bar extends Foo { static a; }",
        );
    }

    #[test]
    fn testSingleClass_withMultipleMemberFields_notAmbiguated() {
        test_transform(
            "
class Foo {
    field = 1;
    ['hi'] = 4;
    static 1 = 2;
    hello = 5;
}",
            "
class Foo {
    a = 1;
    ['hi'] = 4;
    static 1 = 2;
    b = 5;
}",
        );
    }

    #[test]
    fn testQuotedMemberFnInClass_notAmbiguated() {
        test_same("class Foo { 'methodFoo'() {} }");
    }

    // TODO:
    // #[test]
    // fn testQuotedMemberFnInClassReservesPropertyName() {
    //   test_transform(
    //       "/** @unrestricted */ class Foo { 'a'() {} foo() {} }",
    //       "/** @unrestricted */ class Foo { 'a'() {} b() {} }");
    // }

    #[test]
    fn testSingleClass_withTwoMemberFns_notAmbiguated() {
        test_transform(
            "class Foo { method1() {} method2() {} }",
            "class Foo { a() {} b() {} }",
        );
    }

    #[test]
    fn testSingleClass_withStaticAndPrototypeMemberFns_ambiguated() {
        test_transform(
            "class Foo { static method1() {} method2() {} }",
            "class Foo { static a() {} a() {} }",
        );
    }

    #[test]
    fn testTwoUnrelatedClasses_withMemberFns_ambiguated() {
        test_transform(
            "class Foo { methodFoo() {} } class Bar { methodBar() {} }",
            "class Foo { a() {} } class Bar { a() {} }",
        );
    }

    #[test]
    fn testTwoUnrelatedClasses_withStaticMemberFns_ambiguated() {
        test_transform(
            "
class Foo { static methodFoo() {} }
class Bar { static methodBar() {} }",
            "
class Foo { static a() {} }
class Bar { static a() {} }",
        );
    }

    #[test]
    fn testEs6SuperclassStaticMethod_notAmbiguated() {
        test_transform(
            "
class Foo { static methodFoo() {} }
class Bar extends Foo { static methodBar() {} }",
            // Since someone could access Foo.methodFoo() through Bar, make sure the methods get
            // distinct names.
            "
class Foo { static b() {} }
class Bar extends Foo { static a() {} }",
        );
    }

    #[test]
    fn testEs6SubclassChain_withStaticMethods_notAmbiguated() {
        test_transform(
            "
class Foo { static methodFoo() { alert('foo'); } }
class Bar extends Foo { static methodBar() {alert('bar'); } }
class Baz extends Bar { static methodBaz() {alert('baz'); } }
class Moo extends Baz { static methodMoo() { alert('moo'); } }
Moo.methodFoo();",
            // All four static methods must get distinct names, so that Moo.a resolves correctly to
            // Foo.a
            "
class Foo { static a() {alert('foo'); } }
class Bar extends Foo { static b() {alert('bar'); } }
class Baz extends Bar { static c() {alert('baz'); } }
class Moo extends Baz { static d() { alert('moo'); } }
Moo.a();",
        );
    }

    // TODO:
    // #[test]
    // fn testEs5ClassWithExtendsChainStaticMethods_notAmbiguated() {
    //   test_transform(
    //       lines(
    //           "/** @constructor */ function Foo () {}",
    //           "Foo.methodFoo = function() { alert('foo'); };",
    //           "class Bar extends Foo { static methodBar() {alert('bar'); } }",
    //           "class Baz extends Bar { static methodBaz() {alert('baz'); } }",
    //           "class Moo extends Baz { static methodMoo() { alert('moo'); } }",
    //           "Moo.methodFoo();"),
    //       lines(
    //           "/** @constructor */ function Foo () {}",
    //           "Foo.a = function() { alert('foo'); };",
    //           "class Bar extends Foo { static b() {alert('bar'); } }",
    //           "class Baz extends Bar { static c() {alert('baz'); } }",
    //           "class Moo extends Baz { static d() { alert('moo'); } }",
    //           "Moo.a();"));
    // }

    // TODO:
    // #[test]
    // fn testEs5ClassWithEs5SubclassWtaticMethods_ambiguated() {
    //   test_transform(
    //       lines(
    //           "/** @constructor */ function Foo () {}",
    //           "Foo.methodFoo = function() { alert('foo'); };",
    //           "/** @constructor @extends {Foo} */ function Bar() {}",
    //           "Bar.methodBar = function() { alert('bar'); };"),
    //       lines(
    //           "/** @constructor */ function Foo () {}",
    //           "Foo.a = function() { alert('foo'); };",
    //           "/** @constructor @extends {Foo} */ function Bar() {}",
    //           "Bar.a = function() { alert('bar'); };"));
    // }

    #[test]
    fn testClassWithSuperclassStaticMethodsCalledWithSuper_ambiguated() {
        test_transform(
            "
class Foo { static methodFoo() {} }
class Bar extends Foo { static methodBar() { super.methodFoo(); } }",
            // Since someone could access Foo.methodFoo() through Bar, make sure the methods get
            // distinct names.
            "
class Foo { static a() {} }
class Bar extends Foo { static b() { super.a(); } }",
        );
    }

    #[test]
    fn testGetterInClass_ambiguated() {
        test_transform("class Foo { get prop() {} }", "class Foo { get a() {} }");
    }

    #[test]
    fn testQuotedGetterInClass_isNotAmbiguated() {
        test_same("class Foo { get 'prop'() {} }");
    }

    // TODO:
    // #[test]
    // fn testQuotedGetterInClassReservesPropertyName() {
    //   test_transform(
    //       "/** @unrestricted */ class Foo { get 'a'() {} foo() {} }",
    //       "/** @unrestricted */ class Foo { get 'a'() {} b() {} }");
    // }

    // TODO:
    // #[test]
    // fn testSetterInClass_isAmbiguated() {
    //     test_transform("class Foo { set prop(x) {} }", "class Foo { set a(x) {} }");
    // }

    // TODO:
    // #[test]
    // fn testQuotedSetterInClass_notAmbiguated() {
    //     test_same("class Foo { set 'prop'(x) {} }");
    // }

    // TODO:
    // #[test]
    // fn test_sameGetterAndSetterInClass_ambiguated() {
    //     test_transform(
    //         "class Foo { get prop() {} set prop(x) {} }",
    //         "class Foo { get a() {} set a(x) {} }",
    //     );
    // }

    // TODO:
    // #[test]
    // fn testDistinctGetterAndSetterInClass_notAmbiguated() {
    //     test_transform(
    //         "class Foo { set propA(x) {} get propB() {} }",
    //         "class Foo { set a(x) {} get b() {} }",
    //     );
    // }

    #[test]
    fn testComputedMemberFunctionInClass_notAmbiguated() {
        test_same("class Foo { ['method']() {}}");
    }

    // TODO:
    // #[test]
    // fn testSimpleComputedMemberFnInClassReservesPropertyName() {
    //     test_transform(
    //         "class Foo { ['a']() {} foo() {} }",
    //         "class Foo { ['a']() {} b() {} }",
    //     );
    // }

    // TODO:
    // #[test]
    // fn testComplexComputedMemberFnInClassDoesntReservePropertyName() {
    //     // we don't try to evaluate 'a' + '' to see that it's identical to 'a', and so end up renaming
    //     // 'foo' -> 'a'
    //     // The property name invalidation is already just a heuristic, so just handle the very simple
    //     // case of ['a']() {}
    //     test_transform(
    //         "class Foo { ['a' + '']() {} foo() {} }",
    //         "class Foo { ['a' + '']() {} a() {} }",
    //     );
    // }

    #[test]
    fn testEs6ClassConstructorMethod_notAmbiguated() {
        test_same("class Foo { constructor() {} }");
    }

    // TODO:
    // #[test]
    // fn testOptchainGet_ambiguated() {
    //   test_transform("class Foo { foo() { this?.foo(); } };", "class Foo { a() { this?.a(); } };");
    // }

    // TODO:
    // #[test]
    // fn testAmbiguateEs6ClassMethodsDoesntCrashOnClassInACast() {
    //   // the cast causes the actual CLASS node to have the unknown type, so verify that the pass
    //   // can handle it not being a function type.
    //   test_same(
    //       lines(
    //           "const Foo = /** @type {?} */ (class {", //
    //           "  method() {}",
    //           "});",
    //           "class Bar {",
    //           "  method() {}",
    //           "}"));
    // }

    // TODO:
    // #[test]
    // fn testObjectSetPrototypeOfIsIgnored() {
    //   test_transform(
    //       externs("Object.setPrototypeOf = function(obj, proto) {}"),
    //       srcs(
    //           lines(
    //               "/** @constructor */",
    //               "function Foo() {}",
    //               "Foo.fooMethod = () => 3;",
    //               "/** @constructor */",
    //               "function Bar() {}",
    //               "Bar.barMethod = () => 4;",
    //               "Object.setPrototypeOf(Foo, Bar);")),
    //       expected(
    //           lines(
    //               "/** @constructor */",
    //               "function Foo() {}",
    //               "Foo.a = () => 3;",
    //               "/** @constructor */",
    //               "function Bar() {}",
    //               "Bar.a = () => 4;",
    //               "Object.setPrototypeOf(Foo, Bar);")));
    //   // now trying to reference Foo.barMethod will not work, and will call barMethod instead.
    //   // AmbiguateProperties currently ignores this case
    // }

    // TODO:
    // #[test]
    // fn testComputedPropertyInObjectLiteral_notAmbiguated() {
    //   test_same("const obj = {['a']: 3, b: 4}");
    // }

    // TODO:
    // #[test]
    // fn testQuotedPropertyInObjectLiteralReservesPropertyName() {
    //   test_transform(
    //       "const obj = {'a': 3}; class C { method() {}}",
    //       // `method` is renamed to `b`, not `a`, to avoid colliding with the computed prop.
    //       // This is just a heuristic; JSCompiler cannot statically determine all string
    //       // property names.
    //       "const obj = {'a': 3}; class C { b() {}}");
    // }

    // TODO:
    // #[test]
    // fn testQuotedMemberFnInObjectLiteralReservesPropertyName() {
    //   test_transform(
    //       "const obj = {'a'() {}}; class C { method() {}}",
    //       "const obj = {'a'() {}}; class C { b() {}}");
    // }

    // TODO:
    // #[test]
    // fn testComputedPropertyInObjectLiteralReservesPropertyName() {
    //   test_transform(
    //       "const obj = {['a']: 3}; class C { method() {}}",
    //       "const obj = {['a']: 3}; class C { b() {}}");
    // }

    // TODO:
    // #[test]
    // fn testMemberFnInObjectLiteralPreventsPropertyRenaming() {
    //   // Don't rename the class member 'm' because the object literal type is invalidating, and
    //   // also has a property 'm'
    //   test_same("const obj = {m() {}}; class C {m() {}}");
    // }

    // TODO:
    // #[test]
    // fn testSimplePropInObjectLiteralPreventsPropertyRenaminge() {
    //   // Don't rename the class member 'm' because the object literal type is invalidating, and
    //   // also has a property 'm'
    //   test_same("const obj = {m: 0}; class C {m() {}}");
    // }

    // TODO:
    // #[test]
    // fn testObjectPatternDeclarationWithStringKey_ambiguated() {
    //   test_transform(
    //       lines(
    //           "class Foo {", //
    //           "  method() {}",
    //           "}",
    //           "const {method} = new Foo();"),
    //       lines(
    //           "class Foo {", //
    //           "  a() {}",
    //           "}",
    //           "const {a: method} = new Foo();"));
    // }

    // TODO:
    // #[test]
    // fn testObjectPatternDeclarationWithStringKeWithDefault_ambiguated() {
    //   test_transform(
    //       lines(
    //           "class Foo {", //
    //           "  method() {}",
    //           "}",
    //           "const {method = () => 3} = new Foo();"),
    //       lines(
    //           "class Foo {", //
    //           "  a() {}",
    //           "}",
    //           "const {a: method = () => 3} = new Foo();"));
    // }

    // TODO:
    // #[test]
    // fn testNestedObjectPatternWithStringKey_ambiguated() {
    //   test_transform(
    //       lines(
    //           "class Foo {", //
    //           "  method() {}",
    //           "}",
    //           "const {foo: {method}} = {foo: new Foo()};"),
    //       lines(
    //           "class Foo {", //
    //           "  a() {}",
    //           "}",
    //           // note: we rename the 'method' access but not 'foo', because we don't try ambiguating
    //           // properties on object literal types.
    //           "const {foo: {a: method}} = {foo: new Foo()};"));
    // }

    // TODO:
    // #[test]
    // fn testObjectPatternParameterWithStringKey_ambiguated() {
    //   test_transform(
    //       lines(
    //           "class Foo {", //
    //           "  method() {}",
    //           "}",
    //           "/** @param {!Foo} foo */",
    //           "function f({method}) {}"),
    //       lines(
    //           "class Foo {", //
    //           "  a() {}",
    //           "}",
    //           "/** @param {!Foo} foo */",
    //           "function f({a: method}) {}"));
    // }

    // TODO:
    // #[test]
    // fn testObjectPatternQuotedStringKey_notAmbiguated() {
    //   // this emits a warning for a computed property access on a struct, since property ambiguation
    //   // will break this code.
    //   ignoreWarnings(DiagnosticGroups.CHECK_TYPES);
    //   test_transform(
    //       lines(
    //           "class Foo {", //
    //           "  method() {}",
    //           "}",
    //           "const {'method': method} = new Foo();"),
    //       lines(
    //           "class Foo {", //
    //           "  a() {}",
    //           "}",
    //           "const {'method': method} = new Foo();"));
    // }

    // TODO:
    // #[test]
    // fn testObjectPatternComputedProperty_notAmbiguated() {
    //   // this emits a warning for a computed property access on a struct, since property ambiguation
    //   // will break this code.
    //   ignoreWarnings(DiagnosticGroups.CHECK_TYPES);
    //   test_transform(
    //       lines(
    //           "class Foo {", //
    //           "  method() {}",
    //           "}",
    //           "const {['method']: method} = new Foo();"),
    //       lines(
    //           "class Foo {", //
    //           "  a() {}",
    //           "}",
    //           "const {['method']: method} = new Foo();"));
    // }

    // TODO:
    // #[test]
    // fn testMixinPropertiesNotAmbiguated() {
    //   test_transform(
    //       lines(
    //           "", //
    //           "class A {",
    //           "  constructor() {",
    //           "    this.aProp = 'aProp';",
    //           "  }",
    //           "}",
    //           "",
    //           "/**",
    //           " * @template T",
    //           " * @param {function(new: T)} baseType",
    //           " * @return {?}",
    //           " */",
    //           "function mixinX(baseType) {",
    //           "  return class extends baseType {",
    //           "    constructor() {",
    //           "      super();",
    //           "      this.x = 'x';",
    //           "    }",
    //           "  };",
    //           "}",
    //           "/** @constructor @extends {A} */",
    //           "const BSuper = mixinX(A);",
    //           "",
    //           "class B extends BSuper {",
    //           "  constructor() {",
    //           "    super();",
    //           "    this.bProp = 'bProp';",
    //           "  }",
    //           "}",
    //           "",
    //           ""),
    //       lines(
    //           "", //
    //           "class A {",
    //           "  constructor() {",
    //           "    this.a = 'aProp';",
    //           "  }",
    //           "}",
    //           "",
    //           "/**",
    //           " * @template T",
    //           " * @param {function(new: T)} baseType",
    //           " * @return {?}",
    //           " */",
    //           "function mixinX(baseType) {",
    //           "  return class extends baseType {",
    //           "    constructor() {",
    //           "      super();",
    //           "      this.x = 'x';",
    //           "    }",
    //           "  };",
    //           "}",
    //           "/** @constructor @extends {A} */",
    //           "const BSuper = mixinX(A);",
    //           "",
    //           "class B extends BSuper {",
    //           "  constructor() {",
    //           "    super();",
    //           // Note that ambiguating 'bProp' => 'a' would be incorrect because BSuper extends A via
    //           // the mixin, so 'bProp' would conflict with 'aProp'. JSCompiler doesn't actually track
    //           // that B extends A. Instead it conservatively invalidates all properties on B because
    //           // BSuper is from a mixin, not a class literal.
    //           "    this.bProp = 'bProp';",
    //           "  }",
    //           "}",
    //           "",
    //           ""));
    // }

    // TODO:
    // #[test]
    // fn testMixinPrototypePropertiesNotAmbiguated() {
    //   test_transform(
    //       lines(
    //           "", //
    //           "class A {",
    //           "  aMethod() {}",
    //           "}",
    //           "",
    //           "function mixinX(baseType) {",
    //           "  return class extends baseType {",
    //           "    x() {}",
    //           "  };",
    //           "}",
    //           "/** @constructor @extends {A} */",
    //           "const BSuper = mixinX(A);",
    //           "",
    //           "class B extends BSuper {",
    //           "  bMethod() {}",
    //           "}",
    //           "",
    //           ""),
    //       lines(
    //           "", //
    //           "class A {",
    //           "  a() {}",
    //           "}",
    //           "",
    //           "function mixinX(baseType) {",
    //           "  return class extends baseType {",
    //           "    x() {}",
    //           "  };",
    //           "}",
    //           "/** @constructor @extends {A} */",
    //           "const BSuper = mixinX(A);",
    //           "",
    //           "class B extends BSuper {",
    //           // Note that ambiguating 'bMethod' => 'a' would be incorrect because BSuper extends A
    //           // via the mixin, so 'bMethod' would conflict with 'aMethod'.
    //           "  bMethod() {}",
    //           "}",
    //           "",
    //           ""));
    // }

    // TODO:
    // #[test]
    // fn testMixinConstructorPropertiesNotAmbiguated() {
    //   test_transform(
    //       lines(
    //           "", //
    //           "class A {",
    //           "  static aMethod() {}",
    //           "}",
    //           "",
    //           "/**",
    //           " * @template T",
    //           " * @param {function(new: T)} baseType",
    //           " * @return {?}",
    //           " */",
    //           "function mixinX(baseType) {",
    //           "  return class extends baseType {",
    //           "    static x() {}",
    //           "  };",
    //           "}",
    //           "/** @constructor @extends {A} */",
    //           "const BSuper = mixinX(A);",
    //           "",
    //           "class B extends BSuper {",
    //           "  static bMethod() {}",
    //           "}",
    //           "",
    //           ""),
    //       lines(
    //           "", //
    //           "class A {",
    //           "  static a() {}",
    //           "}",
    //           "",
    //           "/**",
    //           " * @template T",
    //           " * @param {function(new: T)} baseType",
    //           " * @return {?}",
    //           " */",
    //           "function mixinX(baseType) {",
    //           "  return class extends baseType {",
    //           "    static x() {}",
    //           "  };",
    //           "}",
    //           "/** @constructor @extends {A} */",
    //           "const BSuper = mixinX(A);",
    //           "",
    //           "class B extends BSuper {",
    //           "  static bMethod() {}",
    //           "}",
    //           "",
    //           ""));
    // }

    // TODO:
    // #[test]
    // fn testObjectUsedAsMapIsInvalidating() {
    //   ignoreWarnings(DiagnosticGroups.MISSING_PROPERTIES);
    //   test_transform(
    //       lines(
    //           "/** @param {!Object<string>} obj */",
    //           "function f(obj) {",
    //           "  return obj.x;",
    //           "}",
    //           "class OtherClass {",
    //           "  constructor() {",
    //           "    this.y = 0;",
    //           "  }",
    //           "}"),
    //       lines(
    //           "/** @param {!Object<string>} obj */",
    //           "function f(obj) {",
    //           // Can't ambiguate this 'x' to 'a' since we don't track different Object types uniquely
    //           // and OtherClass is also assignable to !Object<string>
    //           "  return obj.x;",
    //           "}",
    //           "class OtherClass {",
    //           "  constructor() {",
    //           "    this.a = 0;",
    //           "  }",
    //           "}"));
    // }

    // TODO:
    // #[test]
    // fn testAmbiguateMethodAddedToStringPrototype_accessedOffScalar() {
    //   test_transform(
    //       lines(
    //           "/** @return {string} */",
    //           "String.prototype.customMethod = function() { return ''; };",
    //           "'FOOBAR'.customMethod();",
    //           "class Bar {",
    //           "  bar() {}",
    //           "}",
    //           "new Bar().bar();"),
    //       lines(
    //           "/** @return {string} */",
    //           "String.prototype.a = function() { return ''; };",
    //           "'FOOBAR'.a();",
    //           "class Bar {",
    //           "  a() {}",
    //           "}",
    //           "new Bar().a();"));
    // }

    // TODO:
    // #[test]
    // fn testAmbiguateMethodAddedToStringPrototype() {
    //   test_transform(
    //       lines(
    //           "/** @return {string} */",
    //           "String.prototype.customMethod = function() { return ''; };",
    //           "new String('FOOBAR').customMethod();",
    //           "class Bar {",
    //           "  bar() {}",
    //           "}",
    //           "new Bar().bar();"),
    //       lines(
    //           "/** @return {string} */",
    //           "String.prototype.a = function() { return ''; };",
    //           "new String('FOOBAR').a();",
    //           "class Bar {",
    //           "  a() {}",
    //           "}",
    //           "new Bar().a();"));
    // }
}
