#![allow(non_snake_case)]
#![warn(non_upper_case_globals)]
#![warn(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![warn(warnings)]
#![warn(dead_code)]
#![deny(unused_imports)]

mod DataFlowAnalysis2;
mod function;
mod graph;
mod hashable_map;
pub mod simple_set;
mod template;
mod types;
pub mod unionfind;
mod utils;

use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::BTreeMap;
use std::convert::TryInto;
use std::hash::{BuildHasherDefault, Hash};
use std::ops::Deref;

use ast::*;
use atoms::{js_word, JsWord};
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::SyntaxContext;
use index::bit_set::{BitMatrix, BitSet, GrowableBitSet};
use index::vec::IndexVec;
use petgraph::algo::TarjanScc;
use petgraph::graph::{DiGraph, Neighbors, NodeIndex, UnGraph};
use petgraph::graphmap::GraphMap;
use petgraph::{Directed, EdgeDirection::*};
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

use crate::control_flow::node::{Node, NodeKind};
use crate::control_flow::ControlFlowAnalysis::*;
use crate::control_flow::ControlFlowGraph::*;
use crate::convert::ecma_number_to_string;
use crate::find_vars::*;
use crate::optimize_properties2::function::StepBuilder;
use crate::utils::unwrap_as;
use crate::DataFlowAnalysis::LatticeElementId;
use crate::DefaultNameGenerator::DefaultNameGenerator;

use simple_set::IndexSet;
use template::*;
use unionfind::UnionFind;
use DataFlowAnalysis2::*;

use self::hashable_map::HashableHashMap;
use self::types::{ObjectId, ObjectStore, UnionBuilder, UnionId, UnionStore};

#[cfg(test)]
mod tests;

fn create_renaming_map(store: &mut Store) -> FxHashMap<NodeId, JsWord> {
    /*
        Idea:
        Assign properties (from different objects but with the same name) to the same
        bucket (equivalence class) if the must share the same name.

        Each property starts in its own bucket. Then, buckets are merged when:
        - One of their properties is accessed on a union of object types (the buckets
          for each object's version of the property are merged).
        - Their property exists on a 'root' and 'child' object. These are created by
          function calls, where each child object inherits the properties of its root.
          These inherited properties must share the same name (they represent the same
          slot), and so their buckets are merged.

        Once we have created all buckets, a representative `Property` is created for each
        bucket.

        These representatives are then added to a graph, and edges are added between
        representatives if they exist on the same object, and therefore must have distinct
        names. E.g. for `{ prop1: 1, prop2: 2 }; { prop3: 3 }`, the graph would have three
        nodes ('prop1', 'prop2', 'prop3'), and an edge between 'prop1' and 'prop2'.

        This graph is then coloured, assigning each representative a colour (number) such
        that no adjacent nodes have the same colour. Representatives with more references
        to their property name are coloured first, so they get lower colours (and the
        shortest names).

        These colours are then assigned names, with the lowest colours getting the shortest
        names.

        Finally, each representative's references are mapped to their new name based on the
        representative's colour.
    */

    #[derive(Default, Debug)]
    struct Object {
        properties: FxHashMap<NameId, PropId>,
    }

    #[derive(Debug, Clone)]
    struct Property {
        name: NameId,
        prop_id: PropId,
        references: FxHashSet<NodeId>,
        invalid: bool,
    }

    index::newtype_index!(struct PropId { .. });

    // TODO:
    // These could be reserve space for built-ins using a loop to calculate the capacities, which
    // the compiler may be able to optimise away, since it's an iteration over a static slice.
    let mut objects: FxHashMap<ObjectId, Object> = FxHashMap::default();
    let mut properties: IndexVec<PropId, Property> = IndexVec::default();

    for builtin in ObjectStore::BUILT_INS {
        if !builtin.properties.is_empty() {
            let obj = objects.entry(builtin.id).or_default();
            let props = builtin.properties.iter().map(|p| {
                let name = store.names.insert(p.clone());
                (
                    name,
                    properties.push(Property {
                        name,
                        prop_id: properties.next_index(),
                        references: FxHashSet::default(),
                        invalid: true,
                    }),
                )
            });
            obj.properties.extend(props);
        }
    }

    let mut union_accesses = FxHashMap::<_, FxHashMap<_, FxHashSet<_>>>::default();

    for (PropKey(name, node_id), &pointer) in &store.references {
        match pointer {
            Pointer::Object(object_id) => {
                let object = objects.entry(object_id).or_default();
                let id = match object.properties.entry(*name) {
                    Entry::Occupied(entry) => *entry.get(),
                    Entry::Vacant(entry) => {
                        let prop_id = properties.push(Property {
                            name: *name,
                            prop_id: properties.next_index(),
                            references: FxHashSet::default(),
                            invalid: store.invalid_objects.contains(object_id),
                        });
                        entry.insert(prop_id);
                        prop_id
                    }
                };
                properties[id].references.insert(*node_id);
            }
            Pointer::Union(union) => {
                // Defer union processing.
                union_accesses
                    .entry(union)
                    .or_default()
                    .entry(name)
                    .or_default()
                    .insert(*node_id);
                for constituent in store.unions[union].constituents() {
                    objects.entry(constituent).or_default();
                }
            }
            Pointer::Fn(_) | Pointer::NullOrVoid => {}
        }
    }

    let mut union_find = UnionFind::new(properties.len());

    // Process super/sub type relations.
    for (super_ty, sub_ty) in &store.object_links {
        objects.entry(*super_ty).or_default();
        objects.entry(*sub_ty).or_default();

        let invalid = store.invalid_objects.contains(*sub_ty);
        let [super_ty, sub_ty] = objects.get_many_mut([super_ty, sub_ty]).unwrap();
        for (name, super_prop) in &super_ty.properties {
            match sub_ty.properties.entry(*name) {
                Entry::Occupied(entry) => {
                    union_find.union(*super_prop, *entry.get());
                }
                Entry::Vacant(entry) => {
                    entry.insert(*super_prop);
                    properties[*super_prop].invalid |= invalid;
                }
            }
        }
    }

    // Process unions.
    for (union, accesses) in union_accesses {
        let invalid = store.invalidated(Pointer::Union(union));

        for (&name, references) in accesses {
            let mut props = store.unions[union]
                .constituents()
                .filter_map(|c| objects.get(&c).and_then(|c| c.properties.get(&name)));

            let representative = if let Some(&representative) = props.next() {
                properties[representative].references.extend(references);
                properties[representative].invalid |= invalid;
                representative
            } else {
                let prop_id = properties.push(Property {
                    name,
                    prop_id: properties.next_index(),
                    references,
                    invalid,
                });
                union_find.add(prop_id);
                prop_id
            };

            for constituent in store.unions[union].constituents() {
                match objects
                    .get_mut(&constituent)
                    .unwrap()
                    .properties
                    .entry(name)
                {
                    Entry::Occupied(entry) => {
                        union_find.union(representative, *entry.get());
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(representative);
                    }
                }
            }
        }
    }

    index::newtype_index!(struct RepId { .. });

    let labeling = union_find.into_labeling();

    // Create representatives for each equivalence class.
    let mut representatives = Vec::new();
    {
        let mut map = FxHashMap::default();
        for obj in objects.values() {
            for &prop in obj.properties.values() {
                let representative = labeling[prop.index()];
                let representative = *map.entry(representative).or_insert_with(|| {
                    let index = representatives.len();
                    representatives.push(properties[representative].clone());
                    index
                });
                let representative = &mut representatives[representative];
                let prop = &properties[prop];
                representative
                    .references
                    .extend(prop.references.iter().copied());
                representative.invalid |= prop.invalid;
            }
        }
    }

    representatives.retain(|r| !r.invalid);

    // Sort representatives so those with more references have lower IDs.
    representatives.sort_unstable_by(|a, b| {
        let result = b.references.len().cmp(&a.references.len());
        if result.is_eq() {
            store.names[a.name].cmp(&store.names[b.name])
        } else {
            result
        }
    });

    // Assign representatives their IDs.
    let representatives: IndexVec<RepId, Property> = IndexVec::from_raw(representatives);
    let representatives_map: FxHashMap<PropId, RepId> = representatives
        .iter_enumerated()
        .map(|(id, prop)| (prop.prop_id, id))
        .collect();

    // Create property graph.
    // TODO: only need to store half/triangle since graph is undirected, and therefore reflexivity is implied.
    let mut graph = BitMatrix::new(representatives.len(), representatives.len());
    for obj in objects.values() {
        let mut outer = obj
            .properties
            .values()
            .filter_map(|p| representatives_map.get(&labeling[p.index()]));
        while let Some(&outer_node) = outer.next() {
            for &inner_node in outer.clone() {
                graph.insert(outer_node, inner_node);
                graph.insert(inner_node, outer_node);
            }
        }
    }

    const DEBUG_GRAPH: bool = false;

    if cfg!(debug_assertions) && DEBUG_GRAPH {
        let mut debug_graph: UnGraph<(RepId, JsWord), ()> = UnGraph::default();
        let mut debug_graph_map = FxHashMap::default();

        for obj in objects.values() {
            if obj.properties.len() == 1 {
                let prop = *obj.properties.values().next().unwrap();
                let rep_id = match representatives_map.get(&labeling[prop.index()]) {
                    Some(r) => *r,
                    None => continue,
                };
                debug_graph_map.entry(rep_id).or_insert_with(|| {
                    let name = store.names[properties[prop].name].clone();
                    debug_graph.add_node((rep_id, name))
                });
                continue;
            }
            let mut outer = obj
                .properties
                .values()
                .filter_map(|p| representatives_map.get(&labeling[p.index()]));
            while let Some(&outer_node) = outer.next() {
                let a = *debug_graph_map.entry(outer_node).or_insert_with(|| {
                    let name = store.names[representatives[outer_node].name].clone();
                    debug_graph.add_node((outer_node, name))
                });
                for &inner_node in outer.clone() {
                    let b = *debug_graph_map.entry(inner_node).or_insert_with(|| {
                        let name = store.names[representatives[inner_node].name].clone();
                        debug_graph.add_node((inner_node, name))
                    });
                    debug_graph.update_edge(a, b, ());
                }
            }
        }

        let dot = format!(
            "{:?}",
            petgraph::dot::Dot::with_config(&debug_graph, &[petgraph::dot::Config::EdgeNoLabel])
        );

        std::fs::write("props.dot", dot).expect("Failed to output prop graph");
    }

    // Colour property graph:
    // Try to assign a colour to representatives, from most to least references
    // (lower to higher IDs). A colour can be assigned if none of the
    // representative's neighbours have the colour already. Once no more
    // representatives can be assigned the colour, move on the the next, until
    // no uncoloured representatives remain.
    let mut remaining_nodes = BitSet::<RepId>::new_filled(representatives.len());
    let mut colours = vec![0u16; representatives.len()];
    let mut cur_colour = 0u16;
    let mut subgraph = BitSet::new_empty(representatives.len());
    loop {
        subgraph.clear();
        // This is essentially just `BitSet::iter` inlined, allowing us to modify
        // the set as we go (see `BitIter` for details).
        let mut cur_word_offset = 0;
        let mut offset = 0;
        while cur_word_offset < remaining_nodes.words().len() {
            let mut word = remaining_nodes.words()[cur_word_offset];

            while word != 0 {
                // Get the position of the next set bit in the current word,
                // then clear the bit.
                let bit_pos = word.trailing_zeros() as usize;
                let bit = 1 << bit_pos;
                word ^= bit;
                let node = RepId::from_usize(bit_pos + offset);
                if !graph.iter(node).any(|n| subgraph.contains(n)) {
                    subgraph.insert(node);
                    colours[node.index()] = cur_colour;
                    remaining_nodes.remove(node);
                }
            }

            cur_word_offset += 1;
            offset += index::bit_set::WORD_BITS;
        }
        cur_colour += 1;

        if remaining_nodes.is_empty() {
            break;
        }
    }

    // Generate new names for the properties that will be renamed.
    let mut name_gen = DefaultNameGenerator::new(FxHashSet::default());
    let mut colour_map = Vec::with_capacity(cur_colour as usize);
    for _ in 0..cur_colour {
        colour_map.push(name_gen.generate_next_name());
    }

    let mut rename_map = FxHashMap::default();

    for (id, representative) in representatives.into_iter_enumerated() {
        let colour = colours[id.index()];
        let new_name = &colour_map[colour as usize];
        for reference in representative.references {
            rename_map.insert(reference, new_name.clone());
        }
    }

    rename_map
}

static DONE_VAR_COUNT: std::sync::Mutex<Vec<usize>> = std::sync::Mutex::new(Vec::new());

pub fn analyse(ast: &ast::Program, unresolved_ctxt: SyntaxContext) -> Store<'_> {
    let mut store = Store {
        calls: IndexSet::default(),
        functions: IndexVec::default(),
        static_fn_data: IndexVec::default(),
        function_map: FxHashMap::default(),
        objects_map: FxHashMap::default(),
        invalid_objects: GrowableBitSet::new_empty(),
        unions: UnionStore::default(),
        references: FxHashMap::default(),
        objects: ObjectStore::new(),
        unresolved_ctxt,
        resolved_calls: FxHashMap::default(),
        call_templates: IndexVec::default(),
        fn_assignments: HashableHashMap::default(),
        object_links: FxHashSet::default(),
        call_objects: FxHashMap::default(),
        call_resolver_state: ResolverState::default(),
        names: IndexSet::default(),
        vars: IndexSet::default(),

        accessed_props: FxHashMap::default(),

        always_invalid_vars: FxHashSet::default(),
    };

    let mut fn_vars = FxHashSet::default();

    {
        let mut v = DeclFinder {
            names: &mut store.names,
            vars: &mut store.vars,
            var_start: VarId::from_u32(0),
            fn_vars: &mut fn_vars,
        };
        ast.visit_with(&mut v);
    }
    let mut good_functions = FxHashSet::default();
    let mut fn_dependencies = FxHashMap::default();

    {
        let mut visitor = FnVisitor {
            store: &mut store,
            cur_var_start: VarId::from_u32(0),
            function_stack: Vec::new(),
            fn_vars: &mut fn_vars,

            fn_dependencies: &mut fn_dependencies,
            good_functions: &mut good_functions,
        };
        ast.visit_with(&mut visitor);
    }

    find_always_invalid_vars(
        &mut store.always_invalid_vars,
        ast,
        &mut store.names,
        &mut store.vars,
        &store.static_fn_data,
        unresolved_ctxt,
    );

    let mut step_builder = StepBuilder::new();

    let mut fn_graph = GraphMap::default();

    let call_templates = store
        .functions
        .indices()
        .map(|func| {
            CallTemplate::new(
                &store.static_fn_data,
                store.unresolved_ctxt,
                func,
                &store.function_map,
                &mut store.names,
                &mut store.vars,
                &mut step_builder,
                &mut store.unions,
                &mut store.objects_map,
                &mut store.objects,
                &mut store.invalid_objects,
                &store.fn_assignments,
                &mut fn_graph,
                &store.always_invalid_vars,
            )
        })
        .collect();
    store.call_templates = IndexVec::from_raw(call_templates);

    // let mut dot = format!("{:?}", petgraph::dot::Dot::with_config(&fn_graph, &[]));

    // std::fs::write("./fg.dot", dot).expect("Failed to output fn graph");

    // let mut num_simple_templates = 0;
    // let mut num_simple_with_props_templates = 0;
    // let mut num_complex_templates = 0;

    // for t in store.call_templates.iter() {
    //     match t {
    //         CallTemplate::Steps(_) => {
    //             num_complex_templates += 1;
    //         }
    //         CallTemplate::Simple(_) => {
    //             num_simple_templates += 1;
    //         }
    //         CallTemplate::SimpleWithProps(_, _) => {
    //             num_simple_with_props_templates += 1;
    //         }
    //     }
    // }

    // println!("num_simple_templates: {}", num_simple_templates);
    // println!(
    //     "num_simple_with_props_templates: {}",
    //     num_simple_with_props_templates
    // );
    // println!("num_complex_templates: {}", num_complex_templates);

    // panic!();

    let mut done_objects = GrowableBitSet::new_empty();
    let mut done_functions = BitSet::new_empty(store.functions.len());
    let mut done_vars = BitSet::new_empty(store.vars.len());

    let mut fn_graph: GraphMap<FnId, (), Directed, BuildHasherDefault<FxHasher>> =
        GraphMap::with_capacity_and_hasher(good_functions.len(), 0, Default::default());

    for func in &good_functions {
        fn_graph.add_node(*func);
    }

    fn get_dep<'a>(dep: &'a FnDep, fn_assignments: &'a FxHashMap<VarId, FnId>) -> &'a FnId {
        match dep {
            FnDep::Var(v) => fn_assignments.get(v).unwrap(),
            FnDep::Fn(f) => f,
        }
    }

    for (func, deps) in fn_dependencies.iter() {
        if !good_functions.contains(func) {
            continue;
        }
        let bad = deps
            .iter()
            .any(|d| !good_functions.contains(get_dep(d, &store.fn_assignments)));

        if bad {
            good_functions.remove(func);
            fn_graph.remove_node(*func);
            continue;
        }

        for dep in deps {
            let dep = get_dep(dep, &store.fn_assignments);
            fn_graph.add_edge(*func, *dep, ());
        }
    }

    let mut tarjan = TarjanScc::default();

    tarjan.run(&fn_graph, |scc| {
        debug_assert!(scc.iter().all(|func| good_functions.contains(func)));

        let bad = scc.iter().any(|func| {
            fn_dependencies
                .get(func)
                .iter()
                .copied()
                .flatten()
                .any(|d| !good_functions.contains(get_dep(d, &store.fn_assignments)))
        });
        if bad {
            for func in scc {
                good_functions.remove(func);
            }
        } else {
            let mut func_props = FxHashSet::default();
            for &func in scc {
                if let CallTemplate::Steps(steps) = &store.call_templates[func] {
                    for (start, end) in &steps.map {
                        let steps = {
                            let (start, end) = (*start as usize, *end as usize);
                            if start > steps.steps.len() || end == start {
                                continue;
                            }
                            &steps.steps[start..end]
                        };
                        let mut cur_l_value_prop = None;
                        for step in steps {
                            let prop = match step {
                                function::Step::StoreRValue(Some(v)) => match v {
                                    function::RValue::Prop(p) => Some(p),
                                    _ => None,
                                },
                                function::Step::StoreLValue(Some(v)) => match v {
                                    function::LValue::RValueProp(p) => {
                                        cur_l_value_prop = Some(p);
                                        None
                                    }
                                    _ => {
                                        cur_l_value_prop = None;
                                        None
                                    }
                                },
                                function::Step::Assign(conditional) => {
                                    if *conditional {
                                        if let Some(prop) = cur_l_value_prop {
                                            func_props.insert(*prop);
                                        }
                                        cur_l_value_prop = None;
                                    }
                                    None
                                }
                                function::Step::InvalidateLValue => {
                                    cur_l_value_prop = None;
                                    None
                                }
                                _ => None,
                            };
                            if let Some(prop) = prop {
                                func_props.insert(*prop);
                            }
                        }
                    }

                    for dep in fn_graph.neighbors_directed(func, Outgoing) {
                        if dep == func {
                            continue;
                        }

                        if let Some(dep_props) = store.accessed_props.get(&dep) {
                            debug_assert!(!scc.contains(&dep));
                            func_props.extend(dep_props.iter().copied());
                        }
                    }
                }
            }

            for &func in scc.iter().rev().skip(1) {
                store.accessed_props.insert(func, func_props.clone());
            }
            if let Some(func) = scc.last() {
                store.accessed_props.insert(*func, func_props);
            }
        }
    });

    let root = match &ast {
        Program::Module(n) => ControlFlowRoot::Module(n),
        Program::Script(n) => ControlFlowRoot::Script(n),
    };
    let cfa = ControlFlowAnalysis::analyze(root, false);

    let data_flow_analysis = DataFlowAnalysis::new(
        cfa.cfg,
        &cfa.node_priorities,
        false,
        &mut done_objects,
        &mut done_functions,
        &mut done_vars,
    );

    let mut analysis = Analysis { data_flow_analysis };
    analysis.data_flow_analysis.analyze(&mut store);

    // {
    //     let func = FnId::from_u32(3773);
    //     let static_data = &store.static_fn_data[func];

    //     let cfa = ControlFlowAnalysisResult {
    //         cfg: ControlFlowGraph {
    //             map: static_data.cfg.map.clone(),
    //             implicit_return: static_data.cfg.implicit_return,
    //             implicit_return_index: static_data.cfg.implicit_return_index,
    //             entry: static_data.cfg.entry,
    //             entry_index: static_data.cfg.entry_index,
    //             graph: static_data.cfg.graph.clone(),
    //             node_annotations: FxHashMap::default(),
    //             edge_annotations: FxHashMap::default(),
    //         },
    //         nodePriorities: static_data.node_priorities.clone(),
    //     };

    //     let data_flow_analysis =
    //         DataFlowAnalysis::new(cfa.cfg, &cfa.nodePriorities, true, Some(func));

    //     let mut analysis = Analysis { data_flow_analysis };
    //     analysis.data_flow_analysis.analyze(&mut store);

    //     // let matching_calls = store
    //     //     .calls
    //     //     .into_iter()
    //     //     .filter(|c| c.func == func)
    //     //     .take(100)
    //     //     .collect::<Vec<_>>();

    //     dbg!(&store.calls);

    //     panic!();
    // }

    // println!("Analysing {} functions:", store.functions.len());

    let mut i = 0;
    while i < store.functions.len() {
        // if i > 4000 {
        //     {
        //         let mut counts: FxHashMap<usize, usize> = FxHashMap::default();
        //         for call in &store.calls {
        //             let names = call
        //                 .state
        //                 .prop_assignments
        //                 .keys()
        //                 .map(|(_, n)| *n)
        //                 .collect::<FxHashSet<_>>();
        //             *counts.entry(names.len()).or_default() += 1;
        //         }
        //         let mut counts = counts.into_iter().collect::<Vec<_>>();
        //         counts.sort_unstable();
        //         dbg!(counts);
        //     }

        //     {
        //         let counts = DONE_VAR_COUNT.lock().unwrap();
        //         let counts = counts
        //             .iter()
        //             .enumerate()
        //             .filter(|(_, c)| **c != 0)
        //             .collect::<Vec<_>>();
        //         dbg!(counts);
        //     }

        //     panic!();
        // }

        let static_data = &store.static_fn_data[i.into()];

        let cfa = ControlFlowAnalysisResult {
            cfg: ControlFlowGraph {
                map: static_data.cfg.map.clone(),
                implicit_return: static_data.cfg.implicit_return,
                implicit_return_index: static_data.cfg.implicit_return_index,
                entry: static_data.cfg.entry,
                entry_index: static_data.cfg.entry_index,
                graph: static_data.cfg.graph.clone(),
                node_annotations: FxHashMap::default(),
                edge_annotations: FxHashMap::default(),
            },
            node_priorities: static_data.node_priorities.clone(),
        };

        let data_flow_analysis = DataFlowAnalysis::new(
            cfa.cfg,
            &cfa.node_priorities,
            true,
            &mut done_objects,
            &mut done_functions,
            &mut done_vars,
        );

        let mut analysis = Analysis { data_flow_analysis };
        analysis.data_flow_analysis.analyze(&mut store);
        // println!("Analysed function {}/{}", i, store.functions.len());
        i += 1;
    }

    // println!("Analysed {} functions:", store.functions.len());

    // {
    //     let mut counts: FxHashMap<usize, usize> = FxHashMap::default();
    //     for call in &store.calls {
    //         let names = call
    //             .state
    //             .prop_assignments
    //             .keys()
    //             .map(|(_, n)| *n)
    //             .collect::<FxHashSet<_>>();
    //         *counts.entry(names.len()).or_default() += 1;
    //     }
    //     let mut counts = counts.into_iter().collect::<Vec<_>>();
    //     counts.sort_unstable();
    //     dbg!(counts);
    // }

    // {
    //     let counts = DONE_VAR_COUNT.lock().unwrap();
    //     let counts = counts
    //         .iter()
    //         .enumerate()
    //         .filter(|(_, c)| **c != 0)
    //         .collect::<Vec<_>>();
    //     dbg!(counts);
    // }

    debug_assert!(!store.invalid_objects.contains(ObjectStore::RESOLVING_CALL));
    debug_assert!(!store.invalid_objects.contains(ObjectStore::NUMBER));
    debug_assert!(!store.invalid_objects.contains(ObjectStore::STRING));
    debug_assert!(!store.invalid_objects.contains(ObjectStore::BOOL));
    debug_assert!(!store.invalid_objects.contains(ObjectStore::BIG_INT));

    debug_assert!(!store.vars.into_iter().any(|v| v.1 == unresolved_ctxt));

    store
}

pub fn process(
    ast: &mut ast::Program,
    program_data: &mut ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    let mut store = analyse(ast, unresolved_ctxt);

    let rename_map = create_renaming_map(&mut store);

    // Actually assign the new names.
    let mut renamer = Renamer {
        program_data,
        rename_map,
    };

    ast.visit_mut_with(&mut renamer);
}

enum FnDep {
    Var(VarId),
    Fn(FnId),
}

fn find_always_invalid_vars(
    always_invalid_vars: &mut FxHashSet<VarId>,
    ast: &ast::Program,
    names: &mut IndexSet<NameId, JsWord>,
    vars: &mut IndexSet<VarId, Id>,
    static_fn_data: &IndexVec<FnId, StaticFunctionData>,
    unresolved_ctxt: SyntaxContext,
) {
    let mut candidates = find_const_vars(ast, names, vars);

    for data in static_fn_data {
        for v in &data.captured_vars {
            candidates.remove(v);
        }
    }

    let mut v = AlwaysInvalidVarFinder {
        names,
        vars,
        unresolved_ctxt,

        candidates: &candidates,

        always_invalid_vars,
    };
    ast.visit_with(&mut v);
}

struct AlwaysInvalidVarFinder<'a> {
    names: &'a mut IndexSet<NameId, JsWord>,
    vars: &'a mut IndexSet<VarId, Id>,
    unresolved_ctxt: SyntaxContext,

    candidates: &'a FxHashSet<VarId>,

    always_invalid_vars: &'a mut FxHashSet<VarId>,
}

impl<'ast> Visit<'ast> for AlwaysInvalidVarFinder<'_> {
    fn visit_member_expr(&mut self, node: &'ast MemberExpr) {
        if !is_simple_member_expr_prop(&node.prop, self.unresolved_ctxt, node.computed) {
            if let ExprOrSuper::Expr(obj) = &node.obj {
                if let Expr::Ident(obj) = obj.as_ref() {
                    if obj.ctxt != self.unresolved_ctxt {
                        let id = Id::new(obj, &mut self.names);
                        let name = self.vars.get_index(&id).unwrap();

                        if self.candidates.contains(&name) {
                            self.always_invalid_vars.insert(name);
                        }
                    }
                }
            }
        }
        node.visit_children_with(self);
    }

    fn visit_assign_expr(&mut self, node: &'ast AssignExpr) {
        if let Expr::Ident(rhs) = &*node.right {
            if rhs.ctxt != self.unresolved_ctxt {
                let unresolved_lhs = match &node.left {
                    PatOrExpr::Expr(lhs) => is_unresolvable_lhs(lhs, self.unresolved_ctxt),
                    PatOrExpr::Pat(lhs) => match lhs.as_ref() {
                        Pat::Ident(lhs) => lhs.id.ctxt == self.unresolved_ctxt,
                        Pat::Expr(lhs) => is_unresolvable_lhs(lhs, self.unresolved_ctxt),
                        _ => false,
                    },
                };

                if unresolved_lhs {
                    let id = Id::new(rhs, &mut self.names);
                    let name = self.vars.get_index(&id).unwrap();

                    if self.candidates.contains(&name) {
                        self.always_invalid_vars.insert(name);
                    }
                }
            }
        }
        node.visit_children_with(self);
    }

    fn visit_call_expr(&mut self, node: &'ast CallExpr) {
        if let ExprOrSuper::Expr(callee) = &node.callee {
            if is_unresolvable_lhs(callee, self.unresolved_ctxt) {
                for arg in &node.args {
                    if let ExprOrSpread::Expr(arg) = arg {
                        if let Expr::Ident(arg) = arg.as_ref() {
                            if arg.ctxt != self.unresolved_ctxt {
                                let id = Id::new(arg, &mut self.names);
                                let name = self.vars.get_index(&id).unwrap();

                                if self.candidates.contains(&name) {
                                    self.always_invalid_vars.insert(name);
                                }
                            }
                        }
                    }
                }
            }
        }

        node.visit_children_with(self);
    }

    fn visit_new_expr(&mut self, node: &'ast NewExpr) {
        if let Expr::Ident(callee) = node.callee.as_ref() {
            if callee.ctxt != self.unresolved_ctxt {
                let id = Id::new(callee, &mut self.names);
                let name = self.vars.get_index(&id).unwrap();

                if self.candidates.contains(&name) {
                    self.always_invalid_vars.insert(name);
                }
            }
        }

        if let Some(args) = &node.args {
            for arg in args {
                if let ExprOrSpread::Expr(arg) = arg {
                    if let Expr::Ident(arg) = arg.as_ref() {
                        if arg.ctxt != self.unresolved_ctxt {
                            let id = Id::new(arg, &mut self.names);
                            let name = self.vars.get_index(&id).unwrap();

                            if self.candidates.contains(&name) {
                                self.always_invalid_vars.insert(name);
                            }
                        }
                    }
                }
            }
        }

        node.visit_children_with(self);
    }

    fn visit_yield_expr(&mut self, node: &'ast YieldExpr) {
        if let Some(arg) = &node.arg {
            if let Expr::Ident(arg) = arg.as_ref() {
                if arg.ctxt != self.unresolved_ctxt {
                    let id = Id::new(arg, &mut self.names);
                    let name = self.vars.get_index(&id).unwrap();

                    if self.candidates.contains(&name) {
                        self.always_invalid_vars.insert(name);
                    }
                }
            }
        }

        node.visit_children_with(self);
    }
}

fn is_simple_member_expr_prop(prop: &Expr, unresolved_ctxt: SyntaxContext, computed: bool) -> bool {
    match prop {
        Expr::Lit(e) => match e {
            Lit::Str(_) | Lit::Bool(_) | Lit::Null(_) | Lit::Num(_) | Lit::BigInt(_) => true,
            Lit::Regex(_) | Lit::JSXText(_) => false,
        },
        Expr::Ident(e) => {
            if computed {
                e.ctxt == unresolved_ctxt
                    && (e.sym == js_word!("undefined") || e.sym == js_word!("NaN"))
            } else {
                true
            }
        }
        _ => false,
    }
}

fn is_unresolvable_lhs(mut lhs: &Expr, unresolved_ctxt: SyntaxContext) -> bool {
    loop {
        match lhs {
            Expr::Member(e) => {
                if !is_simple_member_expr_prop(&e.prop, unresolved_ctxt, e.computed) {
                    return true;
                }
                if let ExprOrSuper::Expr(e) = &e.obj {
                    lhs = &e;
                    continue;
                }
            }
            Expr::Call(e) => {
                if let ExprOrSuper::Expr(e) = &e.callee {
                    lhs = &e;
                    continue;
                }
            }
            Expr::Ident(e) => {
                if e.ctxt == unresolved_ctxt {
                    return true;
                }
            }
            Expr::Paren(e) => {
                lhs = &e.expr;
                continue;
            }
            // TODO:
            // Expr::PrivateName(_) => todo!(),
            // Expr::OptChain(_) => todo!(),
            _ => {}
        }
        return false;
    }
}

fn find_const_vars(
    ast: &ast::Program,
    names: &mut IndexSet<NameId, JsWord>,
    vars: &mut IndexSet<VarId, Id>,
) -> FxHashSet<VarId> {
    let mut v = ConstVarFinder {
        names,
        vars,

        assignment_counts: FxHashMap::default(),
    };
    ast.visit_with(&mut v);

    let mut assigned_once = FxHashSet::default();

    for (v, assignments) in v.assignment_counts {
        if assignments == AssignmentInfo::Const {
            assigned_once.insert(v);
        }
    }

    assigned_once
}

#[derive(PartialEq, Eq)]
enum AssignmentInfo {
    Const,
    Reassigned,
    Function,
}

struct ConstVarFinder<'a> {
    names: &'a mut IndexSet<NameId, JsWord>,
    vars: &'a mut IndexSet<VarId, Id>,

    assignment_counts: FxHashMap<VarId, AssignmentInfo>,
}

impl ConstVarFinder<'_> {
    fn record_var(&mut self, ident: &Ident) {
        let id = Id::new(ident, self.names);
        let var = self.vars.insert(id);

        match self.assignment_counts.entry(var) {
            Entry::Occupied(mut entry) => {
                match entry.get() {
                    AssignmentInfo::Const => {
                        entry.insert(AssignmentInfo::Reassigned);
                    }
                    // Terminal states remain unchanged.
                    AssignmentInfo::Reassigned | AssignmentInfo::Function => {}
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(AssignmentInfo::Const);
            }
        }
    }
}

impl<'ast> Visit<'ast> for ConstVarFinder<'_> {
    // Function names are in scope.
    fn visit_fn_decl(&mut self, node: &FnDecl) {
        let id = Id::new(&node.ident, self.names);
        let var = self.vars.insert(id);
        self.assignment_counts.insert(var, AssignmentInfo::Function);
    }

    fn visit_binding_ident(&mut self, node: &BindingIdent) {
        self.record_var(&node.id);
    }

    // The key of AssignPatProp is LHS but is an Ident, so won't be caught by
    // the BindingIdent visitor above.
    fn visit_assign_pat_prop(&mut self, p: &AssignPatProp) {
        self.record_var(&p.key);
    }
}

struct DeclFinder<'a> {
    names: &'a mut IndexSet<NameId, JsWord>,
    vars: &'a mut IndexSet<VarId, Id>,

    var_start: VarId,

    fn_vars: &'a mut FxHashSet<VarId>,
}

impl DeclFinder<'_> {
    fn record_var(&mut self, ident: &Ident, fn_name: bool) {
        let id = Id::new(ident, self.names);
        // Var should not have been previously defined (unless it was within the
        // current function).
        debug_assert!(
            !self.vars.contains(&id) || self.vars.get_index(&id).unwrap() >= self.var_start
        );
        let var = self.vars.insert(id);
        if fn_name {
            self.fn_vars.insert(var);
        } else {
            self.fn_vars.remove(&var);
        }
    }
}

impl<'ast> Visit<'ast> for DeclFinder<'_> {
    // Don't visit nested functions.
    fn visit_function(&mut self, _node: &Function) {}
    fn visit_constructor(&mut self, _node: &Constructor) {}
    fn visit_arrow_expr(&mut self, _node: &ArrowExpr) {}
    fn visit_getter_prop(&mut self, _node: &GetterProp) {}
    fn visit_setter_prop(&mut self, _node: &SetterProp) {}

    // Function names are in scope.
    fn visit_fn_decl(&mut self, node: &FnDecl) {
        self.record_var(&node.ident, true);
    }

    // Expressions can't declare new vars.
    fn visit_expr(&mut self, _node: &Expr) {}
    fn visit_prop_name(&mut self, _node: &PropName) {}

    fn visit_binding_ident(&mut self, node: &BindingIdent) {
        self.record_var(&node.id, false);
    }

    // The key of AssignPatProp is LHS but is an Ident, so won't be caught by
    // the BindingIdent visitor above.
    fn visit_assign_pat_prop(&mut self, p: &AssignPatProp) {
        self.record_var(&p.key, false);
    }
}

struct FnVisitor<'ast, 's> {
    store: &'s mut Store<'ast>,
    cur_var_start: VarId,
    function_stack: Vec<FnId>,
    fn_vars: &'s mut FxHashSet<VarId>,

    fn_dependencies: &'s mut FxHashMap<FnId, Vec<FnDep>>,
    good_functions: &'s mut FxHashSet<FnId>,
}

impl<'ast> FnVisitor<'ast, '_> {
    fn handle_fn<T>(&mut self, node: &'ast T, fn_expr_name: Option<&'ast Ident>) -> FnId
    where
        T: FunctionLike<'ast> + GetNodeId,
        &'ast T: Into<ControlFlowRoot<'ast>>,
        Node<'ast>: From<&'ast T>,
    {
        debug_assert!(!self.store.function_map.contains_key(&node.node_id()));

        let cfa =
            ControlFlowAnalysis::<DummyAnnotation, DummyAnnotation>::analyze(node.into(), false);

        // cfa.cfg.print_full();
        let cfg = SimpleCFG {
            map: cfa.cfg.map,
            implicit_return: cfa.cfg.implicit_return,
            implicit_return_index: cfa.cfg.implicit_return_index,
            entry: cfa.cfg.entry,
            entry_index: cfa.cfg.entry_index,
            graph: cfa.cfg.graph,
        };

        let var_start = self.store.vars.len().try_into().unwrap();

        let mut v = DeclFinder {
            names: &mut self.store.names,
            vars: &mut self.store.vars,
            var_start: VarId::from_u32(var_start),
            fn_vars: self.fn_vars,
        };

        for param in node.params() {
            if let Pat::Ident(param) = param {
                v.record_var(&param.id, false);
            } else {
                todo!("non-ident param");
            }
        }

        let param_end = v.vars.len().try_into().unwrap();

        // FnExpr's name is local to it. Although the name comes before the
        // params, we record it afterwards to simplify tracking of where
        // vars/params start/end.
        if let Some(fn_expr_name) = fn_expr_name {
            v.record_var(fn_expr_name, true);
        }

        node.visit_body_with(&mut v);

        let static_data = StaticFunctionData {
            cfg,
            node_priorities: cfa.node_priorities,
            var_start,
            param_end,
            captured_vars: Vec::new(),
            accesses_arguments_array: false,
        };

        let func = Func {
            entry_state: Lattice::default(),
        };

        let id = self.store.functions.push(func);
        self.store.function_map.insert(node.node_id(), id);

        let static_id = self.store.static_fn_data.push(static_data);
        debug_assert_eq!(static_id, id);

        self.good_functions.insert(id);

        let old_var_start = self.cur_var_start;
        self.function_stack.push(id);
        self.cur_var_start = VarId::from_u32(var_start);
        node.visit_body_with(self);
        self.function_stack.pop();
        self.cur_var_start = old_var_start;
        id
    }
}

impl<'ast> Visit<'ast> for FnVisitor<'ast, '_> {
    fn visit_ident(&mut self, node: &'ast Ident) {
        if let Some(func) = self.function_stack.last() {
            if node.sym == js_word!("arguments") && node.ctxt == self.store.unresolved_ctxt {
                self.store.static_fn_data[*func].accesses_arguments_array = true;
                return;
            }
        }
        let id = Id::new(node, &mut self.store.names);
        if let Some(id) = self.store.vars.get_index(&id) {
            for &func in self.function_stack.iter().rev() {
                let static_data = &mut self.store.static_fn_data[func];

                if id.as_u32() < static_data.var_start {
                    if let Some(i) = static_data.captured_vars.iter().position(|&e| e >= id) {
                        if static_data.captured_vars[i] == id {
                            // `id` is already in the set.
                        } else {
                            // `id` is smaller than one or more existing elements.
                            static_data.captured_vars.insert(i, id);
                        }
                    } else {
                        // `id` is larger than all existing elements.
                        static_data.captured_vars.push(id);
                    }
                } else {
                    break;
                }
            }
        }

        // let id = Id::new(node, &mut self.store.names);
        // if let Some(id) = self.store.vars.get_index(&id) {
        //     if id < self.cur_var_start {
        //         self.store.invalid_vars.insert(id);
        //     }
        // }
    }

    fn visit_call_expr(&mut self, node: &'ast CallExpr) {
        node.visit_children_with(self);

        if let Some(func) = self.function_stack.last() {
            if let ExprOrSuper::Expr(callee) = &node.callee {
                if let Expr::Ident(callee) = callee.as_ref() {
                    if self.good_functions.contains(func) {
                        if callee.ctxt != self.store.unresolved_ctxt {
                            let id = Id::new(callee, &mut self.store.names);
                            let name = self.store.vars.get_index(&id).unwrap();
                            let is_fn_var = self.fn_vars.contains(&name);
                            if is_fn_var {
                                self.fn_dependencies
                                    .entry(*func)
                                    .or_default()
                                    .push(FnDep::Var(name));
                                return;
                            }
                        }
                    }
                } else {
                    let mut expr = callee.as_ref();
                    let mut callee = None;
                    loop {
                        match expr {
                            Expr::Fn(e) => {
                                callee = self.store.function_map.get(&e.function.node_id);
                            }
                            Expr::Seq(e) => {
                                if let Some(last) = e.exprs.last() {
                                    expr = last;
                                    continue;
                                }
                            }
                            Expr::Arrow(e) => {
                                callee = self.store.function_map.get(&e.node_id);
                            }
                            Expr::Paren(e) => {
                                expr = &e.expr;
                                continue;
                            }
                            _ => {}
                        }
                        break;
                    }
                    if let Some(callee) = callee {
                        if self.good_functions.contains(func) {
                            self.fn_dependencies
                                .entry(*func)
                                .or_default()
                                .push(FnDep::Fn(*callee));
                            return;
                        }
                    }
                }
            }
            self.good_functions.remove(func);
        }
    }

    fn visit_fn_decl(&mut self, node: &'ast FnDecl) {
        let id = self.handle_fn(&node.function, None);
        let name = Id::new(&node.ident, &mut self.store.names);
        let name = self.store.vars.get_index(&name).unwrap();
        self.store.fn_assignments.insert(name, id);
    }
    fn visit_fn_expr(&mut self, node: &'ast FnExpr) {
        self.handle_fn(&node.function, node.ident.as_ref());
    }
    fn visit_function(&mut self, node: &'ast Function) {
        self.handle_fn(node, None);
    }
    fn visit_constructor(&mut self, node: &'ast Constructor) {
        self.handle_fn(node, None);
    }
    fn visit_arrow_expr(&mut self, node: &'ast ArrowExpr) {
        self.handle_fn(node, None);
    }
    fn visit_getter_prop(&mut self, node: &'ast GetterProp) {
        self.handle_fn(node, None);
    }
    fn visit_setter_prop(&mut self, node: &'ast SetterProp) {
        self.handle_fn(node, None);
    }
}

#[derive(Debug)]
struct Analysis<'ast, 'p> {
    data_flow_analysis: DataFlowAnalysis<'ast, 'p>,
}

#[derive(Debug)]
/// Info collected during analysis.
pub struct Store<'ast> {
    calls: IndexSet<CallId, Call>,
    pub(super) functions: IndexVec<FnId, Func>,
    static_fn_data: IndexVec<FnId, StaticFunctionData<'ast>>,
    function_map: FxHashMap<NodeId, FnId>,
    objects_map: FxHashMap<NodeId, ObjectId>,
    invalid_objects: GrowableBitSet<ObjectId>,
    unions: UnionStore,

    references: FxHashMap<PropKey, Pointer>,

    objects: ObjectStore,

    pub(super) unresolved_ctxt: SyntaxContext,

    resolved_calls: FxHashMap<CallId, ResolvedCall>,
    /// Read-only
    call_templates: IndexVec<FnId, CallTemplate>,
    fn_assignments: HashableHashMap<VarId, FnId>,

    object_links: FxHashSet<(ObjectId, ObjectId)>,
    call_objects: FxHashMap<(CallId, NodeId), ObjectId>,

    call_resolver_state: ResolverState,

    names: IndexSet<NameId, JsWord>,
    vars: IndexSet<VarId, Id>,

    accessed_props: FxHashMap<FnId, FxHashSet<NameId>>,

    always_invalid_vars: FxHashSet<VarId>,
}

impl Store<'_> {
    /// Returns a [`Pointer`] to a union of `pointer1`'s type and `pointer2`'s type.
    fn create_union(
        &mut self,
        pointer1: Option<Pointer>,
        pointer2: Option<Pointer>,
    ) -> Option<Pointer> {
        create_union(&mut self.unions, pointer1, pointer2, &self.invalid_objects)
    }

    /// Recursively invalidates the entity that `pointer` points to.
    fn invalidate(&mut self, pointer: Option<Pointer>, lattice: &Lattice) {
        invalidate(
            &mut self.invalid_objects,
            &self.unions,
            pointer,
            &lattice.prop_assignments,
        );
    }

    /// Returns true if `pointer` points to an invalid object.
    fn invalidated(&self, pointer: Pointer) -> bool {
        invalidated(pointer, &self.invalid_objects, &self.unions)
    }
}

fn depends_on_unresolved_call(pointer: Option<Pointer>, unions: &UnionStore) -> bool {
    match pointer {
        Some(Pointer::Object(o)) => o == ObjectStore::RESOLVING_CALL,
        Some(Pointer::Union(union)) => unions[union].contains(ObjectStore::RESOLVING_CALL),
        Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => false,
    }
}

/// Returns true if `pointer` points to an invalid object.
fn invalidated(
    pointer: Pointer,
    invalid_objects: &GrowableBitSet<ObjectId>,
    unions: &UnionStore,
) -> bool {
    match pointer {
        Pointer::Object(obj) => invalid_objects.contains(obj),
        Pointer::Union(union) => {
            union.invalid()
                || unions[union]
                    .constituents()
                    .any(|obj| invalid_objects.contains(obj))
        }
        Pointer::Fn(_) => true,
        Pointer::NullOrVoid => false,
    }
}

/// Returns true if `pointer` points to an invalid object.
fn fully_invalidated(
    pointer: Option<Pointer>,
    invalid_objects: &GrowableBitSet<ObjectId>,
    unions: &UnionStore,
) -> bool {
    match pointer {
        Some(Pointer::Object(obj)) => invalid_objects.contains(obj),
        Some(Pointer::Union(union)) => unions[union]
            .constituents()
            .all(|obj| invalid_objects.contains(obj)),
        Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) => false,
        None => true,
    }
}

/// Returns a [`Pointer`] to a union of `pointer1`'s type and `pointer2`'s type.
fn create_union(
    unions: &mut UnionStore,
    pointer1: Option<Pointer>,
    pointer2: Option<Pointer>,
    invalid_objects: &GrowableBitSet<ObjectId>,
) -> Option<Pointer> {
    if pointer1 == pointer2 {
        return pointer1;
    }

    let mut builder = UnionBuilder::default();

    builder.add(pointer1, unions, invalid_objects);
    builder.add(pointer2, unions, invalid_objects);

    unions.build_union(builder)
}

/// Recursively invalidates the entity that `pointer` points to.
fn invalidate(
    invalid_objects: &mut GrowableBitSet<ObjectId>,
    unions: &UnionStore,
    pointer: Option<Pointer>,
    prop_assignments: &PropertyAssignments,
) {
    let mut queue = match pointer {
        Some(Pointer::Object(o)) if o.is_built_in() => return,
        None | Some(Pointer::Fn(_) | Pointer::NullOrVoid) => return,
        Some(Pointer::Object(o)) => {
            vec![o]
        }
        Some(Pointer::Union(u)) => unions[u].constituents().collect(),
    };

    while let Some(obj) = queue.pop() {
        if obj.is_built_in() {
            continue;
        }

        let new_invalidation = invalid_objects.insert(obj);

        if new_invalidation {
            for (_, prop) in prop_assignments.range((obj, NameId::from_u32(0))..(obj, NameId::MAX))
            {
                match prop.rhs {
                    Some(Pointer::Object(o)) => {
                        if !o.is_built_in() && !invalid_objects.contains(o) {
                            queue.push(o);
                        }
                    }
                    Some(Pointer::Union(u)) => queue.extend(
                        unions[u]
                            .constituents()
                            .filter(|c| !c.is_built_in() && !invalid_objects.contains(*c)),
                    ),
                    None | Some(Pointer::Fn(_) | Pointer::NullOrVoid) => continue,
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
/// Points to an abstraction of a javascript object.
pub(self) enum Pointer {
    Object(ObjectId),
    Union(UnionId),
    Fn(FnId),
    NullOrVoid,
}

index::newtype_index!(pub(super) struct FnId { .. });
index::newtype_index!(pub(super) struct CallId { .. });

index::newtype_index!(pub(super) struct NameId { .. });

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct Id(pub NameId, pub SyntaxContext);

impl Id {
    pub fn new(ident: &Ident, names: &mut IndexSet<NameId, JsWord>) -> Self {
        Self(names.insert(ident.sym.clone()), ident.ctxt)
    }
}

#[derive(Debug)]
pub(super) struct SimpleCFG<'ast> {
    map: FxHashMap<Node<'ast>, NodeIndex>,
    implicit_return: Node<'ast>,
    implicit_return_index: NodeIndex,
    entry: Node<'ast>,
    entry_index: NodeIndex,
    graph: DiGraph<Node<'ast>, Branch>,
}

impl<'ast> SimpleCFG<'ast> {
    pub fn get_successors(&self, node: Node<'ast>) -> Neighbors<'_, Branch> {
        self.graph
            .neighbors_directed(*self.map.get(&node).unwrap(), Outgoing)
    }
}

#[derive(Debug)]
pub(super) struct Func {
    entry_state: Lattice,
}

#[derive(Debug)]
pub(super) struct StaticFunctionData<'ast> {
    cfg: SimpleCFG<'ast>,
    node_priorities: Vec<NodePriority>,
    var_start: u32,
    param_end: u32,
    captured_vars: Vec<VarId>,
    accesses_arguments_array: bool,
}

impl StaticFunctionData<'_> {
    fn param_indices(&self) -> impl Iterator<Item = VarId> {
        VarId::from_u32(self.var_start)..VarId::from_u32(self.param_end)
    }

    fn param_count(&self) -> usize {
        (self.param_end - self.var_start) as usize
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) struct Call {
    func: FnId,
    // args: CallArgs,
    state: Lattice,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum CallArgs {
    Heap(Vec<Option<Pointer>>),
    Invalid(usize),
}

impl CallArgs {
    fn new() -> Self {
        CallArgs::Invalid(0)
    }

    fn push(
        &mut self,
        mut arg: Option<Pointer>,
        func: &StaticFunctionData,
        invalid_objects: &mut GrowableBitSet<ObjectId>,
        unions: &UnionStore,
        prop_assignments: &PropertyAssignments,
    ) {
        if fully_invalidated(arg, invalid_objects, unions) {
            arg = None;
        }

        if arg.is_some() {
            let param_index = match self {
                CallArgs::Heap(args) => args.len(),
                CallArgs::Invalid(none_count) => *none_count,
            };
            if func.accesses_arguments_array || param_index >= func.param_count() {
                arg = None;
                invalidate(invalid_objects, unions, arg, prop_assignments);
            }
        }

        match self {
            CallArgs::Heap(args) => {
                args.push(arg);
            }
            CallArgs::Invalid(none_count) => {
                if arg.is_none() {
                    *none_count += 1;
                } else {
                    let mut new = Vec::new();
                    new.extend(std::iter::repeat(None).take(*none_count));
                    new.push(arg);
                    *self = CallArgs::Heap(new);
                }
            }
        }
    }
}
// #[derive(Debug, PartialEq, Eq, Hash)]
// enum CallArgs {
//     Heap(Box<[Option<Pointer>]>),
//     // TODO: change this to simple run length encoding. e.g. Repeated(Option<Pointer>, u32) i.e. Repeated(pointer, count)
//     /// Number of consecutive `None`s
//     Invalid(usize),
// }

// impl CallArgs {
//     fn get(&self, index: usize) -> Option<Option<Pointer>> {
//         match self {
//             CallArgs::Heap(args) => args.get(index).copied(),
//             CallArgs::Invalid(len) => {
//                 if index >= *len {
//                     None
//                 } else {
//                     Some(None)
//                 }
//             }
//         }
//     }
// }

// #[derive(Debug)]
// struct CallArgBuilder {
//     len: usize,
//     args: Option<Vec<Option<Pointer>>>,
//     none_count: usize,
// }

// impl CallArgBuilder {
//     fn new(len: usize) -> Self {
//         Self {
//             len,
//             args: None,
//             none_count: 0,
//         }
//     }

//     fn push(
//         &mut self,
//         mut arg: Option<Pointer>,
//         func: &StaticFunctionData,
//         invalid_objects: &GrowableBitSet<ObjectId>,
//         invalid_vars: &GrowableBitSet<VarId>,
//     ) {
//         match arg {
//             Some(Pointer::Object(o)) => {
//                 if invalid_objects.contains(o) {
//                     arg = None;
//                 }
//             }
//             Some(Pointer::Union(union)) => {
//                 if union.invalid() {
//                     // todo:
//                     arg = None;
//                 }
//             }
//             Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => {}
//         }

//         // TODO: should we skip args that would go past the function's param count?

//         if arg.is_some() {
//             let param_index = if let Some(args) = &self.args {
//                 args.len()
//             } else {
//                 self.none_count
//             };
//             if let Some(param_var_id) = func.param_indices().nth(param_index) {
//                 if invalid_vars.contains(param_var_id) {
//                     arg = None;
//                 }
//             } else {
//                 arg = None;
//             }
//         }

//         if let Some(args) = &mut self.args {
//             args.push(arg);
//         } else if arg.is_none() {
//             self.none_count += 1;
//         } else {
//             let mut new = Vec::new();
//             new.reserve_exact(self.len);
//             new.extend(std::iter::repeat(None).take(self.none_count));
//             new.push(arg);
//             self.args = Some(new);
//         }
//     }

//     fn finish(self) -> CallArgs {
//         match self.args {
//             Some(args) => {
//                 debug_assert_eq!(self.len, args.len());
//                 debug_assert!(self.len > 0);
//                 CallArgs::Heap(args.into())
//             }
//             None => {
//                 debug_assert_eq!(self.len, self.none_count);
//                 CallArgs::Invalid(self.none_count)
//             }
//         }
//     }
// }

#[derive(Debug, PartialEq)]
pub(super) struct ResolvedCall {
    return_type: Option<Pointer>,
    prop_assignments: PropertyAssignments,
    captured_vars: HashableHashMap<VarId, Assignment>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct Assignment {
    rhs: Option<Pointer>,
}

/// A place where a variable can be stored.
#[derive(Debug, Clone, Copy)]
enum Slot {
    Var(VarId),
    /// (Object, Property name)
    Prop(Pointer, NameId),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub(super) struct PropKey(pub NameId, pub NodeId);

impl PropKey {
    pub fn from_prop_name(
        prop: &PropName,
        unresolved_ctxt: SyntaxContext,
        names: &mut IndexSet<NameId, JsWord>,
    ) -> Option<PropKey> {
        match prop {
            PropName::Ident(p) => Some(PropKey(names.insert(p.sym.clone()), p.node_id)),
            PropName::Str(p) => Some(PropKey(names.insert(p.value.clone()), p.node_id)),
            PropName::Num(p) => Some(PropKey(
                names.insert(ecma_number_to_string(p.value).into()),
                p.node_id,
            )),
            PropName::Computed(p) => PropKey::from_expr(&p.expr, unresolved_ctxt, true, names),
        }
    }

    pub fn from_expr(
        expr: &Expr,
        unresolved_ctxt: SyntaxContext,
        computed: bool,
        names: &mut IndexSet<NameId, JsWord>,
    ) -> Option<PropKey> {
        match expr {
            Expr::Ident(e) => {
                if !computed
                    || e.ctxt == unresolved_ctxt
                        && (e.sym == js_word!("undefined") || e.sym == js_word!("NaN"))
                {
                    // TODO: is this wrong? For "const fooVar = 'a'; obj[foovar]" this will record the prop name as 'foovar' when it is reall 'a'
                    Some(PropKey(names.insert(e.sym.clone()), e.node_id))
                } else {
                    None
                }
            }
            Expr::Lit(e) => match e {
                Lit::Str(e) => Some(PropKey(names.insert(e.value.clone()), e.node_id)),
                Lit::Bool(e) => {
                    if e.value {
                        Some(PropKey(names.insert(js_word!("true")), e.node_id))
                    } else {
                        Some(PropKey(names.insert(js_word!("false")), e.node_id))
                    }
                }
                Lit::Null(e) => Some(PropKey(names.insert(js_word!("null")), e.node_id)),
                Lit::Num(e) => Some(PropKey(
                    names.insert(ecma_number_to_string(e.value).into()),
                    e.node_id,
                )),
                Lit::BigInt(e) => Some(PropKey(
                    names.insert(e.value.to_str_radix(10).into()),
                    e.node_id,
                )),
                Lit::Regex(_) | Lit::JSXText(_) => None,
            },
            _ => None,
        }
    }
}

/// Returns true if the string value of the [`PropName`] is statically determinable.
pub fn is_simple_prop_name(prop_name: &PropName, unresolved_ctxt: SyntaxContext) -> bool {
    match prop_name {
        // TODO: is this wrong? For "const fooVar = 'a'; obj[foovar]" this will record the prop name as 'foovar' when it is reall 'a'
        PropName::Ident(_) | PropName::Str(_) | PropName::Num(_) => true,
        PropName::Computed(p) => match p.expr.as_ref() {
            Expr::Lit(e) => match e {
                Lit::Str(_) | Lit::Bool(_) | Lit::Null(_) | Lit::Num(_) | Lit::BigInt(_) => true,
                Lit::Regex(_) | Lit::JSXText(_) => false,
            },
            Expr::Ident(e) => {
                e.ctxt == unresolved_ctxt
                    && (e.sym == js_word!("undefined") || e.sym == js_word!("NaN"))
            }
            _ => false,
        },
    }
}

fn get_property(
    lattice: &Lattice,
    unions: &mut UnionStore,
    pointer: Pointer,
    key: NameId,
    invalid_objects: &mut GrowableBitSet<ObjectId>,
) -> Option<Pointer> {
    let invalid = match pointer {
        Pointer::Object(obj) => invalid_objects.contains(obj),
        Pointer::Union(_) => {
            if invalidated(pointer, invalid_objects, unions) {
                invalidate(
                    invalid_objects,
                    unions,
                    Some(pointer),
                    &lattice.prop_assignments,
                );
                true
            } else {
                false
            }
        }
        Pointer::Fn(_) | Pointer::NullOrVoid => true,
    };
    if invalid {
        if cfg!(debug_assertions) {
            match pointer {
                Pointer::Object(obj) => {
                    let prop = lattice
                        .prop_assignments
                        .get(&(obj, key))
                        .map(|a| a.rhs)
                        .unwrap_or_default();
                    debug_assert!(
                        matches!(prop, Some(Pointer::Object(o)) if o.is_built_in())
                            || !matches!(prop, Some(Pointer::Object(_) | Pointer::Union(_)))
                            || invalidated(prop.unwrap(), invalid_objects, unions)
                    );
                }
                Pointer::Union(union) => {
                    for constituent in unions[union].constituents() {
                        let constituent = lattice
                            .prop_assignments
                            .get(&(constituent, key))
                            .map(|a| a.rhs)
                            .unwrap_or_default();
                        debug_assert!(
                            matches!(constituent, Some(Pointer::Object(o)) if o.is_built_in())
                                || !matches!(
                                    constituent,
                                    Some(Pointer::Object(_) | Pointer::Union(_))
                                )
                                || invalidated(constituent.unwrap(), invalid_objects, unions)
                        );
                    }
                }
                Pointer::Fn(_) | Pointer::NullOrVoid => {}
            }
        }

        return None;
    }
    match pointer {
        Pointer::Object(obj) => lattice
            .prop_assignments
            .get(&(obj, key))
            .map(|a| a.rhs)
            .unwrap_or(Some(Pointer::NullOrVoid)),
        Pointer::Union(union) => {
            let mut builder = UnionBuilder::default();

            for constituent in unions[union].constituents() {
                let constituent = lattice
                    .prop_assignments
                    .get(&(constituent, key))
                    .map(|a| a.rhs)
                    .unwrap_or(Some(Pointer::NullOrVoid));

                builder.add(constituent, unions, invalid_objects);
            }

            unions.build_union(builder)
        }
        Pointer::Fn(_) => None,
        Pointer::NullOrVoid => unreachable!(),
    }
}

#[derive(Debug)]
struct CowLattice<'a>(Cow<'a, Lattice>);

impl Deref for CowLattice<'_> {
    type Target = Lattice;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl CowLattice<'_> {
    fn insert_var_assignment(
        &mut self,
        name: VarId,
        value: Assignment,
        invalid_objects: &mut GrowableBitSet<ObjectId>,
        unions: &UnionStore,
        fn_assignments: &HashableHashMap<VarId, FnId>,
        always_invalid_vars: &FxHashSet<VarId>,
    ) {
        if always_invalid_vars.contains(&name) {
            invalidate(invalid_objects, unions, value.rhs, &self.prop_assignments);
            return;
        }

        debug_assert!(
            !matches!(
                self.0.var_assignments.get(&name).and_then(|a| a.rhs),
                Some(Pointer::Fn(_))
            ) || self.0.var_assignments.get(&name).copied()
                != fn_assignments.get(&name).map(|f| Assignment {
                    rhs: Some(Pointer::Fn(*f)),
                })
        );
        let new = if fully_invalidated(value.rhs, invalid_objects, unions) {
            Assignment { rhs: None }
        } else {
            value
        };

        if let Some(existing) = self.0.var_assignments.get(&name) {
            if *existing == new {
                return;
            }

            // if new.rhs.is_none() && !fn_assignments.contains_key(&name) {
            //     self.0.to_mut().var_assignments.remove(&name);
            //     return;
            // }

            // self.0.to_mut().var_assignments.insert(name, new);
        }
        // else if new.rhs.is_some() || fn_assignments.contains_key(&name) {
        //     self.0.to_mut().var_assignments.insert(name, new);
        // }
        self.0.to_mut().var_assignments.insert(name, new);
    }

    fn insert_prop_assignment(
        &mut self,
        prop: (ObjectId, NameId),
        value: Assignment,
        invalid_objects: &GrowableBitSet<ObjectId>,
        unions: &UnionStore,
    ) {
        let new = if fully_invalidated(value.rhs, invalid_objects, unions) {
            Assignment { rhs: None }
        } else {
            value
        };
        if let Some(existing) = self.0.prop_assignments.get(&prop) {
            if *existing == new {
                return;
            }
        } else if let Some(Pointer::NullOrVoid) = new.rhs {
            // Assigning null/void is the same as having nothing assigned.
            return;
        }

        self.0.to_mut().prop_assignments.insert(prop, new);
    }

    fn get_var(
        &self,
        id: VarId,
        fn_assignments: &HashableHashMap<VarId, FnId>,
        always_invalid_vars: &FxHashSet<VarId>,
    ) -> Option<Assignment> {
        if always_invalid_vars.contains(&id) {
            return Some(Assignment { rhs: None });
        }
        self.0.var_assignments.get(&id).copied().or_else(|| {
            fn_assignments.get(&id).map(|f| Assignment {
                rhs: Some(Pointer::Fn(*f)),
            })
        })
    }
}

#[derive(Debug)]
struct Analyser<'ast, 'a> {
    lattice: CowLattice<'a>,
    store: &'a mut Store<'ast>,
    in_fn: bool,
    done_objects: &'a mut GrowableBitSet<ObjectId>,
    done_functions: &'a mut BitSet<FnId>,
    done_vars: &'a mut BitSet<VarId>,
}

impl<'ast> Analyser<'ast, '_> {
    fn get_property(&mut self, pointer: Pointer, key: NameId) -> Option<Pointer> {
        get_property(
            &self.lattice,
            &mut self.store.unions,
            pointer,
            key,
            &mut self.store.invalid_objects,
        )
    }

    fn reference_prop(&mut self, object: Pointer, key: PropKey) {
        self.store.references.insert(key, object);
    }

    /// Assigns the given value to the [`Slot`] with the given name. Any existing
    /// value is replaced, unless this assignment is conditional, in which case
    /// the slot is assigned a union of the old and new values.
    fn assign_to_slot(
        &mut self,
        slot: Option<Slot>,
        rhs: Option<Pointer>,
        conditional: bool,
    ) -> Option<Pointer> {
        if let Some(slot) = slot {
            let existing = match &slot {
                Slot::Var(name) => self
                    .lattice
                    .get_var(
                        *name,
                        &self.store.fn_assignments,
                        &self.store.always_invalid_vars,
                    )
                    .and_then(|a| a.rhs),
                Slot::Prop(obj, key) => self.get_property(*obj, *key),
            };

            let rhs = if !conditional {
                // supersede
                rhs
            } else {
                // union
                self.store.create_union(existing, rhs)
            };
            let new = Assignment { rhs };
            match slot {
                Slot::Var(name) => {
                    self.lattice.insert_var_assignment(
                        name,
                        new,
                        &mut self.store.invalid_objects,
                        &self.store.unions,
                        &self.store.fn_assignments,
                        &self.store.always_invalid_vars,
                    );
                }
                Slot::Prop(obj, key) => {
                    if self.store.invalidated(obj) {
                        self.store.invalidate(rhs, &self.lattice);
                    } else {
                        match obj {
                            Pointer::Object(obj) => {
                                self.lattice.insert_prop_assignment(
                                    (obj, key),
                                    new,
                                    &self.store.invalid_objects,
                                    &self.store.unions,
                                );
                            }
                            Pointer::Union(union) => {
                                for constituent in self.store.unions[union].constituents() {
                                    self.lattice.insert_prop_assignment(
                                        (constituent, key),
                                        new,
                                        &self.store.invalid_objects,
                                        &self.store.unions,
                                    );
                                }
                            }
                            Pointer::Fn(_) => {
                                unreachable!();
                            }
                            Pointer::NullOrVoid => {}
                        }
                    }
                }
            };
            rhs
        } else {
            // Unknown/invalid assignment target.
            self.store.invalidate(rhs, &self.lattice);
            rhs
        }
    }

    /// Records an assignment of the form `LHS OP RHS` e.g.
    /// ```js
    /// let a = 1
    /// ```
    /// ```js
    /// a.b = c
    /// ```
    /// ```js
    /// a ||= b
    /// ```
    /// ```js
    /// a().b ||= c
    /// ```
    /// ```js
    /// let { a: b } = c
    /// ```
    ///
    /// `conditional` - Whether the entire assignment (LHS and RHS) is conditionally executed.
    fn record_assignment(
        &mut self,
        lhs: Node<'ast>,
        rhs: &'ast Expr,
        conditional: bool,
        op: AssignOp,
    ) -> Option<Pointer> {
        let conditional_assign = matches!(
            op,
            AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
        );
        match lhs.kind {
            NodeKind::ArrayPat(_) | NodeKind::ObjectPat(_) => {
                debug_assert!(!conditional_assign, "invalid assignment target");
                self.handle_destructuring(lhs, rhs, conditional)
            }
            _ => {
                let lhs = self.visit_and_get_slot(lhs, conditional);
                let conditional = conditional || conditional_assign;
                let rhs = self.visit_and_get_object(Node::from(rhs), conditional);

                self.assign_to_slot(lhs, rhs, conditional)
            }
        }
    }

    fn handle_destructuring(
        &mut self,
        lhs: Node<'ast>,
        rhs: &'ast Expr,
        conditional: bool,
    ) -> Option<Pointer> {
        let rhs = self.visit_and_get_object(Node::from(rhs), conditional);
        self.visit_destructuring(lhs, rhs, conditional);
        rhs
    }

    fn invalidate_slot(&mut self, node: Node<'ast>, conditional: bool) {
        if let Some(lhs) = self.visit_and_get_slot(node, conditional) {
            let value = match lhs {
                Slot::Var(name) => self
                    .lattice
                    .get_var(
                        name,
                        &self.store.fn_assignments,
                        &self.store.always_invalid_vars,
                    )
                    .and_then(|a| a.rhs),
                Slot::Prop(obj, key) => self.get_property(obj, key),
            };
            self.store.invalidate(value, &self.lattice);
        }
    }

    fn visit_destructuring(&mut self, lhs: Node<'ast>, rhs: Option<Pointer>, conditional: bool) {
        match lhs.kind {
            NodeKind::ObjectPat(lhs) => {
                let has_complex_props = lhs.props.iter().any(|p| match p {
                    ObjectPatProp::KeyValue(p) => {
                        !is_simple_prop_name(&p.key, self.store.unresolved_ctxt)
                    }
                    ObjectPatProp::Assign(_) | ObjectPatProp::Rest(_) => false,
                });
                if let Some(rhs) = rhs {
                    if has_complex_props {
                        self.store.invalidate(Some(rhs), &self.lattice);
                    }
                    if !self.store.invalidated(rhs) {
                        for prop in &lhs.props {
                            match prop {
                                ObjectPatProp::KeyValue(prop) => {
                                    let key = PropKey::from_prop_name(
                                        &prop.key,
                                        self.store.unresolved_ctxt,
                                        &mut self.store.names,
                                    )
                                    .unwrap();
                                    self.reference_prop(rhs, key);
                                    let rhs_value = self.get_property(rhs, key.0);

                                    if matches!(prop.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                                        let slot = self.visit_and_get_slot(
                                            Node::from(prop.value.as_ref()),
                                            conditional,
                                        );
                                        self.assign_to_slot(slot, rhs_value, conditional);
                                    } else {
                                        self.visit_destructuring(
                                            Node::from(prop.value.as_ref()),
                                            rhs_value,
                                            conditional,
                                        );
                                    }
                                }
                                ObjectPatProp::Assign(_) => {
                                    unreachable!("removed by normalization");
                                }
                                ObjectPatProp::Rest(rest) => {
                                    debug_assert!(lhs.props.last().unwrap() == prop);

                                    // TODO: throw error, don't panic.
                                    // The argument of an object pattern's rest element must be an identifier.
                                    let arg = unwrap_as!(rest.arg.as_ref(), Pat::Ident(i), i);

                                    // TODO: this is imprecise - rest patterns create a new, distinct, object, which has the remaining non-destructured
                                    // properties copied over. These properties must have the same names as those in the original object after renaming.
                                    // But since they are distinct objects, we don't want to conflate them.
                                    let slot = self.get_var_id_from_ident(&arg.id).map(Slot::Var);
                                    self.assign_to_slot(slot, Some(rhs), conditional);
                                }
                            }
                        }
                        return;
                    }
                }
                for prop in &lhs.props {
                    self.visit_destructuring(Node::from(prop), None, conditional);
                }
            }

            NodeKind::BindingIdent(lhs) => {
                let slot = self.get_var_id_from_ident(&lhs.id).map(Slot::Var);
                self.assign_to_slot(slot, rhs, conditional);
            }
            NodeKind::ArrayPat(lhs) => {
                self.store.invalidate(rhs, &self.lattice);
                for element in lhs.elems.iter().filter_map(|e| e.as_ref()) {
                    if let Pat::Expr(elem) = element {
                        self.invalidate_slot(Node::from(elem.as_ref()), conditional);
                    } else {
                        self.visit_destructuring(Node::from(element), None, conditional);
                    }
                }
            }
            NodeKind::RestPat(lhs) => {
                self.store.invalidate(rhs, &self.lattice);
                self.visit_destructuring(Node::from(lhs.arg.as_ref()), None, conditional);
            }
            NodeKind::AssignPat(lhs) => {
                let default_value = self.visit_and_get_object(Node::from(lhs.right.as_ref()), true);

                let result = self.store.create_union(rhs, default_value);

                self.visit_destructuring(Node::from(lhs.left.as_ref()), result, conditional);
            }
            NodeKind::KeyValuePatProp(lhs) => {
                self.store.invalidate(rhs, &self.lattice);
                if matches!(lhs.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                    self.invalidate_slot(Node::from(lhs.value.as_ref()), conditional);
                } else {
                    self.visit_destructuring(Node::from(lhs.value.as_ref()), None, conditional);
                }
            }
            NodeKind::AssignPatProp(_) => {
                unreachable!("removed by normalization");
            }

            _ => {
                dbg!(lhs);
                unreachable!();
            }
        }
    }

    fn get_var_id_from_ident(&mut self, ident: &Ident) -> Option<VarId> {
        if ident.ctxt == self.store.unresolved_ctxt {
            None
        } else {
            let name = self.store.names.get_index(&ident.sym).unwrap();
            let id = Id(name, ident.ctxt);
            let id = self.store.vars.get_index(&id).unwrap();

            Some(id)
        }
    }

    /// Visits the given expression. If the expression resolves to a valid assignment target, a [`Slot`]
    /// representing that target is returned.
    fn visit_and_get_slot(&mut self, node: Node<'ast>, conditional: bool) -> Option<Slot> {
        match node.kind {
            NodeKind::Ident(node) => self.get_var_id_from_ident(node).map(Slot::Var),
            NodeKind::BindingIdent(node) => self.get_var_id_from_ident(&node.id).map(Slot::Var),
            NodeKind::MemberExpr(node) => {
                let obj = self.visit_and_get_object(Node::from(&node.obj), conditional);

                if let Some(prop) = PropKey::from_expr(
                    &node.prop,
                    self.store.unresolved_ctxt,
                    node.computed,
                    &mut self.store.names,
                ) {
                    obj.map(|obj| {
                        self.reference_prop(obj, prop);
                        Slot::Prop(obj, prop.0)
                    })
                } else {
                    self.store.invalidate(obj, &self.lattice);
                    self.visit_and_get_slot(Node::from(node.prop.as_ref()), conditional);
                    None
                }
            }

            // All other nodes cannot evaluate to a reference, and should return None (but we still
            // need to visit their children).
            _ => {
                self.visit_and_get_object(node, conditional);
                None
            }
        }
    }

    fn call_fn(&mut self, func: FnId, args: CallArgs, conditional: bool) -> Option<Pointer> {
        let call = build_call(
            &self.lattice,
            func,
            args,
            &self.store.static_fn_data,
            &mut self.store.unions,
            &mut self.store.invalid_objects,
            &self.store.fn_assignments,
            &mut self.done_objects,
            &mut self.done_functions,
            &self.store.accessed_props,
            &mut self.store.functions,
            &self.store.always_invalid_vars,
        )
        .expect("resolving call object should not be present");

        let call = self.store.calls.insert(call);

        resolve_call(
            call,
            self.store,
            self.done_objects,
            self.done_functions,
            self.done_vars,
        );
        for ((obj, _), prop) in self.lattice.prop_assignments.iter() {
            if self.store.invalid_objects.contains(*obj) {
                invalidate(
                    &mut self.store.invalid_objects,
                    &self.store.unions,
                    prop.rhs,
                    &self.lattice.prop_assignments,
                );
            }
        }

        let resolved = &self.store.resolved_calls[&call];
        let fn_assignments = &self.store.fn_assignments;
        let always_invalid_vars = &self.store.always_invalid_vars;

        apply_call_side_effects(
            &mut self.lattice,
            &resolved.prop_assignments,
            &resolved.captured_vars,
            &mut self.store.unions,
            &mut self.store.invalid_objects,
            conditional,
            |lattice, k, v, invalid_objects, unions| {
                lattice.insert_prop_assignment(k, v, invalid_objects, unions);
            },
            |lattice, k| lattice.prop_assignments.get(k).copied(),
            |lattice, v| lattice.get_var(v, fn_assignments, always_invalid_vars),
            |lattice, name, value, invalid_objects, unions| {
                lattice.insert_var_assignment(
                    name,
                    value,
                    invalid_objects,
                    unions,
                    fn_assignments,
                    always_invalid_vars,
                );
            },
        );

        self.store.resolved_calls[&call].return_type
    }

    fn visit_and_get_object(&mut self, node: Node<'ast>, conditional: bool) -> Option<Pointer> {
        match node.kind {
            NodeKind::FnExpr(f) => {
                let f = *self.store.function_map.get(&f.function.node_id).unwrap();
                // Don't traverse into new control flow nodes.
                Some(Pointer::Fn(f))
            }
            NodeKind::ArrowExpr(_) => {
                let f = *self.store.function_map.get(&node.node_id).unwrap();
                // Don't traverse into new control flow nodes.
                Some(Pointer::Fn(f))
            }

            NodeKind::AssignExpr(node) => {
                self.record_assignment(Node::from(&node.left), &node.right, conditional, node.op)
            }

            NodeKind::Class(_) => todo!(),
            NodeKind::ExtendsClause(_) => todo!(),
            NodeKind::ClassProp(_) => todo!(),
            NodeKind::PrivateProp(_) => todo!(),
            NodeKind::ClassMethod(_) => todo!(),
            NodeKind::PrivateMethod(_) => todo!(),
            NodeKind::Constructor(_) => todo!(),
            NodeKind::VarDeclarator(node) => {
                let lhs = Node::from(&node.name);
                if let Some(rhs) = &node.init {
                    self.record_assignment(lhs, rhs, conditional, AssignOp::Assign);
                } else {
                    self.visit_and_get_object(lhs, conditional);
                }
                None
            }
            NodeKind::ThisExpr(_) => None,
            NodeKind::ArrayLit(node) => {
                for element in &node.elems {
                    if let Some(element) = element {
                        let obj = self.visit_and_get_object(Node::from(element), conditional);
                        // Can't track once it's in the array.
                        self.store.invalidate(obj, &self.lattice);
                    }
                }
                None
            }
            NodeKind::ObjectLit(node) => {
                let object_id = if let Some(existing) = self.store.objects_map.get(&node.node_id) {
                    *existing
                } else {
                    let object_id = self.store.objects.next_object_id();
                    self.store.objects_map.insert(node.node_id, object_id);
                    object_id
                };

                let pointer = Pointer::Object(object_id);

                let is_simple_obj_lit = node.props.iter().all(|p| match p {
                    Prop::KeyValue(p) => is_simple_prop_name(&p.key, self.store.unresolved_ctxt),
                    _ => false,
                });

                if is_simple_obj_lit {
                    for prop in &node.props {
                        let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                        let key = PropKey::from_prop_name(
                            &prop.key,
                            self.store.unresolved_ctxt,
                            &mut self.store.names,
                        )
                        .unwrap();

                        self.reference_prop(pointer, key);

                        let value =
                            self.visit_and_get_object(Node::from(prop.value.as_ref()), conditional);

                        self.assign_to_slot(Some(Slot::Prop(pointer, key.0)), value, conditional);
                    }
                } else {
                    self.store.invalidate(Some(pointer), &self.lattice);
                }
                Some(pointer)
            }
            NodeKind::SpreadElement(node) => {
                let obj = self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
                self.store.invalidate(obj, &self.lattice);
                None
            }
            NodeKind::UnaryExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
                // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-unary-operators
                match node.op {
                    UnaryOp::Plus => Some(Pointer::Object(ObjectStore::NUMBER)),
                    UnaryOp::TypeOf => Some(Pointer::Object(ObjectStore::STRING)),
                    UnaryOp::Void => Some(Pointer::NullOrVoid),
                    UnaryOp::Bang | UnaryOp::Delete => Some(Pointer::Object(ObjectStore::BOOL)),
                    // Output type depends in input type.
                    UnaryOp::Minus | UnaryOp::Tilde => None,
                }
            }
            NodeKind::UpdateExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
                None
            }
            NodeKind::BinExpr(node) => {
                match node.op {
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                        // TODO: if LHS is object, then we know if RHS will execute.
                        let left =
                            self.visit_and_get_object(Node::from(node.left.as_ref()), conditional);
                        let right =
                            self.visit_and_get_object(Node::from(node.right.as_ref()), true);
                        self.store.create_union(left, right)
                    }
                    _ => {
                        self.visit_and_get_object(Node::from(node.left.as_ref()), conditional);
                        self.visit_and_get_object(Node::from(node.right.as_ref()), conditional);
                        match node.op {
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-equality-operators
                            BinaryOp::EqEq
                            | BinaryOp::NotEq
                            | BinaryOp::EqEqEq
                            | BinaryOp::NotEqEq => Some(Pointer::Object(ObjectStore::BOOL)),
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-relational-operators
                            BinaryOp::Lt
                            | BinaryOp::LtEq
                            | BinaryOp::Gt
                            | BinaryOp::GtEq
                            | BinaryOp::In
                            | BinaryOp::InstanceOf => Some(Pointer::Object(ObjectStore::BOOL)),
                            // TODO: we can infer the output type based on the input types.
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-applystringornumericbinaryoperator
                            BinaryOp::LShift
                            | BinaryOp::RShift
                            | BinaryOp::ZeroFillRShift
                            | BinaryOp::Add
                            | BinaryOp::Sub
                            | BinaryOp::Mul
                            | BinaryOp::Div
                            | BinaryOp::Mod
                            | BinaryOp::BitOr
                            | BinaryOp::BitXor
                            | BinaryOp::BitAnd
                            | BinaryOp::Exp => None,

                            BinaryOp::LogicalOr
                            | BinaryOp::LogicalAnd
                            | BinaryOp::NullishCoalescing => unreachable!("handled above"),
                        }
                    }
                }
            }
            NodeKind::ClassExpr(_) => todo!(),
            NodeKind::MemberExpr(node) => {
                let obj = self.visit_and_get_object(Node::from(&node.obj), conditional);
                if let Some(prop) = PropKey::from_expr(
                    &node.prop,
                    self.store.unresolved_ctxt,
                    node.computed,
                    &mut self.store.names,
                ) {
                    if let Some(obj) = obj {
                        self.reference_prop(obj, prop);
                        self.get_property(obj, prop.0)
                    } else {
                        None
                    }
                } else {
                    self.store.invalidate(obj, &self.lattice);
                    self.visit_and_get_object(Node::from(node.prop.as_ref()), conditional);
                    None
                }
            }
            NodeKind::CondExpr(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
                let cons = self.visit_and_get_object(Node::from(node.cons.as_ref()), true);
                let alt = self.visit_and_get_object(Node::from(node.alt.as_ref()), true);
                self.store.create_union(cons, alt)
            }
            NodeKind::CallExpr(node) => {
                let callee = self.visit_and_get_object(Node::from(&node.callee), conditional);
                if let Some(Pointer::Fn(func)) = callee {
                    if let CallTemplate::Simple(return_value) = self.store.call_templates[func] {
                        for arg in &node.args {
                            self.visit_and_get_object(Node::from(arg), conditional);
                        }
                        return return_value;
                    }

                    if let CallTemplate::SimpleWithProps(_, _) = self.store.call_templates[func] {
                        for arg in &node.args {
                            self.visit_and_get_object(Node::from(arg), conditional);
                        }

                        if let CallTemplate::SimpleWithProps(return_value, prop_assignments) =
                            &self.store.call_templates[func]
                        {
                            // todo!("also need to give the caller inner locals that were captured");
                            for ((obj, key), prop) in prop_assignments.iter() {
                                if self.store.invalid_objects.contains(*obj) {
                                    continue;
                                }
                                let key = (*obj, *key);
                                let existing = self
                                    .lattice
                                    .prop_assignments
                                    .get(&key)
                                    .copied()
                                    .and_then(|a| a.rhs);
                                let rhs = if !conditional {
                                    // supersede
                                    prop.rhs
                                } else {
                                    // union
                                    create_union(
                                        &mut self.store.unions,
                                        existing,
                                        prop.rhs,
                                        &self.store.invalid_objects,
                                    )
                                };
                                let new = Assignment { rhs };
                                self.lattice.insert_prop_assignment(
                                    key,
                                    new,
                                    &mut self.store.invalid_objects,
                                    &mut self.store.unions,
                                );
                            }

                            return *return_value;
                        } else {
                            unreachable!();
                        }
                    }

                    let should_invalidate =
                        self.store.static_fn_data[func].accesses_arguments_array;
                    let mut args = CallArgs::new();
                    for arg in &node.args {
                        let arg = self.visit_and_get_object(Node::from(arg), conditional);
                        if should_invalidate {
                            self.store.invalidate(arg, &self.lattice);
                        } else {
                            args.push(
                                arg,
                                &self.store.static_fn_data[func],
                                &mut self.store.invalid_objects,
                                &self.store.unions,
                                &self.lattice.prop_assignments,
                            );
                        }
                    }
                    // TODO: only call whe in_fn is true?
                    self.call_fn(func, args, conditional)
                } else {
                    self.store.invalidate(callee, &self.lattice);

                    for arg in &node.args {
                        let obj = self.visit_and_get_object(Node::from(arg), conditional);
                        self.store.invalidate(obj, &self.lattice);
                    }
                    None
                }
            }
            NodeKind::NewExpr(node) => {
                let callee =
                    self.visit_and_get_object(Node::from(node.callee.as_ref()), conditional);
                self.store.invalidate(callee, &self.lattice);

                if let Some(args) = &node.args {
                    for arg in args {
                        let obj = self.visit_and_get_object(Node::from(arg), conditional);
                        self.store.invalidate(obj, &self.lattice);
                    }
                }
                None
            }
            NodeKind::SeqExpr(node) => {
                debug_assert!(!node.exprs.is_empty());

                let mut i = 0;
                while i < node.exprs.len() - 1 {
                    self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional);
                    i += 1;
                }

                self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional)
            }
            NodeKind::YieldExpr(node) => {
                if let Some(arg) = &node.arg {
                    let arg = self.visit_and_get_object(Node::from(arg.as_ref()), conditional);
                    self.store.invalidate(arg, &self.lattice);
                }
                None
            }
            NodeKind::AwaitExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional)
            }
            NodeKind::Tpl(node) => {
                for expr in &node.exprs {
                    self.visit_and_get_object(Node::from(expr.as_ref()), conditional);
                }
                None
            }
            NodeKind::TaggedTpl(node) => {
                self.visit_and_get_object(Node::from(node.tag.as_ref()), conditional);
                for expr in &node.tpl.exprs {
                    let obj = self.visit_and_get_object(Node::from(expr.as_ref()), conditional);
                    // Expressions in tagged templates can be accessed by the tag function.
                    self.store.invalidate(obj, &self.lattice);
                }
                None
            }
            NodeKind::ParenExpr(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional)
            }
            NodeKind::Super(_) => todo!(),
            NodeKind::OptChainExpr(_) => todo!(),
            NodeKind::Function(_) => todo!(),
            NodeKind::Param(_) => todo!(),
            NodeKind::ParamWithoutDecorators(_) => todo!(),
            NodeKind::BindingIdent(node) => {
                self.visit_and_get_object(Node::from(&node.id), conditional)
            }
            NodeKind::Ident(node) => {
                if node.ctxt == self.store.unresolved_ctxt && node.sym == js_word!("undefined") {
                    Some(Pointer::NullOrVoid)
                } else {
                    self.get_var_id_from_ident(node)
                        .and_then(|id| {
                            self.lattice.get_var(
                                id,
                                &self.store.fn_assignments,
                                &self.store.always_invalid_vars,
                            )
                        })
                        .and_then(|assign| assign.rhs)
                }
            }
            NodeKind::PrivateName(_) => todo!(),

            NodeKind::Null(_) => Some(Pointer::NullOrVoid),

            NodeKind::Str(_) => Some(Pointer::Object(ObjectStore::STRING)),
            NodeKind::Bool(_) => Some(Pointer::Object(ObjectStore::BOOL)),
            NodeKind::Number(_) => Some(Pointer::Object(ObjectStore::NUMBER)),
            NodeKind::BigInt(_) => Some(Pointer::Object(ObjectStore::BIG_INT)),

            NodeKind::Regex(_) | NodeKind::TplElement(_) | NodeKind::MetaPropExpr(_) => None,

            NodeKind::ImportDefaultSpecifier(_) => todo!(),
            NodeKind::ImportStarAsSpecifier(_) => todo!(),
            NodeKind::ImportNamedSpecifier(_) => todo!(),
            NodeKind::ExportNamespaceSpecifier(_) => todo!(),
            NodeKind::ExportDefaultSpecifier(_) => todo!(),
            NodeKind::ExportNamedSpecifier(_) => todo!(),
            NodeKind::ComputedPropName(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
                None
            }
            NodeKind::CatchClause(_) => todo!(),

            // This function is only called on expressions (and their children),
            // so it can't reach e.g. statements. TypeScript and JSX should have
            // been removed by now as well.
            _ => unreachable!(),
        }
    }

    fn init(&mut self, node: Node<'ast>, conditional: bool) {
        match node.kind {
            NodeKind::IfStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::ExprStmt(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
            }
            NodeKind::BlockStmt(_) => {}
            NodeKind::ForStmt(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_object(Node::from(test.as_ref()), conditional);
                }
            }
            NodeKind::Function(node) => {
                let func = *self.store.function_map.get(&node.node_id).unwrap();

                self.lattice =
                    CowLattice(Cow::Owned(self.store.functions[func].entry_state.clone()));
            }
            NodeKind::VarDecl(node) => {
                for decl in &node.decls {
                    self.visit_and_get_object(Node::from(decl), conditional);
                }
            }
            NodeKind::ThisExpr(_)
            | NodeKind::ArrayLit(_)
            | NodeKind::ObjectLit(_)
            | NodeKind::SpreadElement(_)
            | NodeKind::UnaryExpr(_)
            | NodeKind::UpdateExpr(_)
            | NodeKind::BinExpr(_)
            | NodeKind::FnExpr(_)
            | NodeKind::ClassExpr(_)
            | NodeKind::AssignExpr(_)
            | NodeKind::MemberExpr(_)
            | NodeKind::CondExpr(_)
            | NodeKind::CallExpr(_)
            | NodeKind::NewExpr(_)
            | NodeKind::SeqExpr(_)
            | NodeKind::ArrowExpr(_)
            | NodeKind::YieldExpr(_)
            | NodeKind::MetaPropExpr(_)
            | NodeKind::AwaitExpr(_)
            | NodeKind::Tpl(_)
            | NodeKind::TaggedTpl(_)
            | NodeKind::TplElement(_)
            | NodeKind::ParenExpr(_)
            | NodeKind::Super(_)
            | NodeKind::OptChainExpr(_)
            | NodeKind::BindingIdent(_)
            | NodeKind::Ident(_)
            | NodeKind::PrivateName(_) => {
                self.visit_and_get_object(node, conditional);
            }

            NodeKind::ImplicitReturn => {}
            NodeKind::Class(_) => todo!(),
            NodeKind::ExtendsClause(_) => todo!(),
            NodeKind::ClassProp(_) => todo!(),
            NodeKind::PrivateProp(_) => todo!(),
            NodeKind::ClassMethod(_) => todo!(),
            NodeKind::PrivateMethod(_) => todo!(),
            NodeKind::Constructor(_) => todo!(),
            NodeKind::Decorator(_) => todo!(),
            NodeKind::FnDecl(_) => {}
            NodeKind::ClassDecl(_) => todo!(),
            NodeKind::VarDeclarator(_) => todo!(),
            NodeKind::Param(_) => todo!(),
            NodeKind::ParamWithoutDecorators(_) => todo!(),
            NodeKind::ExportDefaultExpr(_) => todo!(),
            NodeKind::ExportDecl(_) => todo!(),
            NodeKind::ImportDecl(_) => todo!(),
            NodeKind::ExportAll(_) => todo!(),
            NodeKind::NamedExport(_) => todo!(),
            NodeKind::ExportDefaultDecl(_) => todo!(),
            NodeKind::ImportDefaultSpecifier(_) => todo!(),
            NodeKind::ImportStarAsSpecifier(_) => todo!(),
            NodeKind::ImportNamedSpecifier(_) => todo!(),
            NodeKind::ExportNamespaceSpecifier(_) => todo!(),
            NodeKind::ExportDefaultSpecifier(_) => todo!(),
            NodeKind::ExportNamedSpecifier(_) => todo!(),
            NodeKind::Script(_) => {}
            NodeKind::Module(_) => {}
            NodeKind::ArrayPat(_) => todo!(),
            NodeKind::ObjectPat(_) => todo!(),
            NodeKind::AssignPat(_) => todo!(),
            NodeKind::RestPat(_) => todo!(),
            NodeKind::KeyValuePatProp(_) => todo!(),
            NodeKind::AssignPatProp(_) => todo!(),
            NodeKind::KeyValueProp(_) => todo!(),
            NodeKind::AssignProp(_) => todo!(),
            NodeKind::GetterProp(_) => {}
            NodeKind::SetterProp(_) => {}
            NodeKind::MethodProp(_) => todo!(),
            NodeKind::ComputedPropName(_) => todo!(),
            NodeKind::SpreadAssignment(_) => todo!(),
            NodeKind::DebuggerStmt(_) => {}
            NodeKind::WithStmt(_) => todo!(),
            NodeKind::ReturnStmt(node) => {
                if let Some(arg) = &node.arg {
                    let obj = self.visit_and_get_object(Node::from(arg.as_ref()), conditional);
                    if !self.in_fn {
                        self.store.invalidate(obj, &self.lattice);
                    }
                }
            }
            NodeKind::LabeledStmt(_) => {}
            NodeKind::SwitchStmt(node) => {
                self.visit_and_get_object(Node::from(node.discriminant.as_ref()), conditional);
            }
            NodeKind::ThrowStmt(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
            }
            NodeKind::TryStmt(_) => {}
            NodeKind::WhileStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::DoWhileStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::ForInStmt(node) => {
                let right = self.visit_and_get_object(Node::from(node.right.as_ref()), conditional);
                // TODO: is this correct? ForIn does not use prop names?
                self.store.invalidate(right, &self.lattice);
            }
            NodeKind::ForOfStmt(_) => todo!(),
            NodeKind::SwitchCase(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_object(Node::from(test.as_ref()), conditional);
                }
            }
            NodeKind::CatchClause(node) => {
                if let Some(param) = &node.param {
                    match param {
                        Pat::Array(_) | Pat::Object(_) => {
                            self.visit_destructuring(Node::from(param), None, conditional);
                        }
                        _ => {
                            self.invalidate_slot(Node::from(param), conditional);
                        }
                    }
                }
            }

            NodeKind::Str(_)
            | NodeKind::Bool(_)
            | NodeKind::Null(_)
            | NodeKind::Number(_)
            | NodeKind::BigInt(_)
            | NodeKind::Regex(_)
            | NodeKind::EmptyStmt(_)
            | NodeKind::BreakStmt(_)
            | NodeKind::ContinueStmt(_) => {}

            _ => unreachable!("{:#?}", node),
        }
    }
}

impl<'ast> DataFlowAnalysis<'ast, '_> {
    fn flow_through(
        &mut self,
        node: Node<'ast>,
        store: &mut Store<'ast>,
        input: LatticeElementId,
        in_fn: bool,
    ) -> LatticeElementId {
        // Make assignments conditional if the node can end abruptly by an exception.
        let conditional = self
            .cfg
            .graph
            .edges(self.cfg.map[&node])
            .any(|e| *e.weight() == Branch::Exception);

        let mut v = Analyser {
            lattice: CowLattice(Cow::Borrowed(&self.lattice_elements[input])),
            store,
            in_fn,
            done_objects: self.done_objects,
            done_functions: self.done_functions,
            done_vars: self.done_vars,
        };

        v.init(node, conditional);

        match v.lattice.0 {
            Cow::Borrowed(_) => {
                // No changes compared to input.
                input
            }
            Cow::Owned(new) => self.add_lattice_element(new),
        }
    }
}

#[derive(Default)]
struct JoinOp {
    result: Lattice,
}

impl<'ast> JoinOp {
    fn join_flow(
        &mut self,
        analysis: &mut DataFlowAnalysis<'ast, '_>,
        store: &mut Store,
        input: LatticeElementId,
    ) {
        let input = &analysis[input];

        // Merge property assignments.
        for ((obj, key), prop) in input.prop_assignments.iter() {
            if store.invalid_objects.contains(*obj) {
                continue;
            }
            match self.result.prop_assignments.entry((*obj, *key)) {
                std::collections::btree_map::Entry::Occupied(entry) => {
                    let union = create_union(
                        &mut store.unions,
                        entry.get().rhs,
                        prop.rhs,
                        &store.invalid_objects,
                    );
                    self.result
                        .prop_assignments
                        .insert((*obj, *key), Assignment { rhs: union });
                }
                std::collections::btree_map::Entry::Vacant(entry) => {
                    entry.insert(*prop);
                }
            }
        }

        if input.var_assignments.len() > self.result.var_assignments.len() {
            let new = input.var_assignments.len() - self.result.var_assignments.len();
            self.result.var_assignments.reserve(new);
        }
        // Merge variable assignments.
        for (&name, assignment) in input.var_assignments.iter() {
            match self.result.var_assignments.entry(name) {
                Entry::Occupied(mut entry) => {
                    let union = create_union(
                        &mut store.unions,
                        entry.get().rhs,
                        assignment.rhs,
                        &store.invalid_objects,
                    );
                    entry.insert(Assignment { rhs: union });
                }
                Entry::Vacant(entry) => {
                    entry.insert(*assignment);
                }
            }
        }
    }

    fn finish(self) -> Lattice {
        self.result
    }
}

// trait LatticeLike {
//     fn insert_prop_assignment(&mut self, key: (ObjectId, NameId), value: Assignment);
//     fn get_property(&self, key: &(ObjectId, NameId))->Assignment;
// }

type PropertyAssignments = BTreeMap<(ObjectId, NameId), Assignment>;

#[derive(Clone, Debug, PartialEq, Default, Hash, Eq)]
pub(super) struct Lattice {
    var_assignments: HashableHashMap<VarId, Assignment>,
    prop_assignments: PropertyAssignments,
}

// impl LatticeLike for Lattice {
//     fn insert_prop_assignment(&mut self, key: (ObjectId, NameId), value: Assignment) {
//         self.insert_prop_assignment(key, value);
//     }

//     fn get_property(&self, key: &(ObjectId, NameId))->Assignment {
//        self.get_property(key)
//     }
// }

// copy resulting values of vars that the fn captured
// copy resulting prop assignments of params/captured vars
fn apply_call_side_effects<L>(
    call_site_state: &mut L,
    prop_assignments: &PropertyAssignments,
    var_assignments: &HashableHashMap<VarId, Assignment>,
    unions: &mut UnionStore,
    invalid_objects: &mut GrowableBitSet<ObjectId>,
    conditional: bool,
    mut insert_prop_assignment: impl FnMut(
        &mut L,
        (ObjectId, NameId),
        Assignment,
        &GrowableBitSet<ObjectId>,
        &UnionStore,
    ),
    get_property: impl Fn(&L, &(ObjectId, NameId)) -> Option<Assignment>,
    get_var_assignment: impl Fn(&L, VarId) -> Option<Assignment>,
    mut insert_var_assignment: impl FnMut(
        &mut L,
        VarId,
        Assignment,
        &mut GrowableBitSet<ObjectId>,
        &UnionStore,
    ),
) {
    if cfg!(debug_assertions) {
        debug_assert!(!prop_assignments
            .keys()
            .any(|(o, _)| *o == ObjectStore::RESOLVING_CALL));
        debug_assert!(!prop_assignments
            .values()
            .any(|p| depends_on_unresolved_call(p.rhs, unions)));
        debug_assert!(!var_assignments
            .values()
            .any(|p| depends_on_unresolved_call(p.rhs, unions)));
    }

    // todo!("also need to give the caller inner locals that were captured");
    for ((obj, key), prop) in prop_assignments.iter() {
        if invalid_objects.contains(*obj) {
            continue;
        }
        let key = (*obj, *key);
        let existing = get_property(call_site_state, &key).and_then(|a| a.rhs);
        let rhs = if !conditional {
            // supersede
            prop.rhs
        } else {
            // union
            create_union(unions, existing, prop.rhs, invalid_objects)
        };
        let new = Assignment { rhs };
        insert_prop_assignment(call_site_state, key, new, invalid_objects, unions);
    }

    for (&name, assignment) in var_assignments.iter() {
        let existing = get_var_assignment(&call_site_state, name).and_then(|a| a.rhs);
        let rhs = if !conditional {
            // supersede
            assignment.rhs
        } else {
            // union
            create_union(unions, existing, assignment.rhs, invalid_objects)
        };
        let new = Assignment { rhs };
        insert_var_assignment(call_site_state, name, new, invalid_objects, unions);
    }
}

// copy/collect vars that the fn captures
// copy prop assignments of params/captured vars
// create Call
fn build_call(
    call_site_state: &Lattice,
    func: FnId,
    args: CallArgs,
    static_fn_data: &IndexVec<FnId, StaticFunctionData>,
    unions: &mut UnionStore,
    invalid_objects: &mut GrowableBitSet<ObjectId>,
    fn_assignments: &HashableHashMap<VarId, FnId>,
    done_objects: &mut GrowableBitSet<ObjectId>,
    done_functions: &mut BitSet<FnId>,
    accessed_props: &FxHashMap<FnId, FxHashSet<NameId>>,
    functions: &mut IndexVec<FnId, Func>,
    always_invalid_vars: &FxHashSet<VarId>,
) -> Result<Call, ()> {
    let mut var_assignments = HashableHashMap::default();
    let mut prop_assignments: PropertyAssignments = PropertyAssignments::default();

    done_objects.clear();
    done_functions.clear();

    let mut done_vars = FxHashSet::default();

    let mut queue = Vec::new();

    queue.push(Pointer::Fn(func));

    match &args {
        CallArgs::Heap(args) => {
            let args = args
                .iter()
                .copied()
                .chain(std::iter::repeat(Some(Pointer::NullOrVoid)));
            for (id, arg) in static_fn_data[func].param_indices().zip(args) {
                if always_invalid_vars.contains(&id) {
                    invalidate(
                        invalid_objects,
                        unions,
                        arg,
                        &call_site_state.prop_assignments,
                    );
                    continue;
                }
                match arg {
                    Some(p @ Pointer::Object(_) | p @ Pointer::Union(_)) => {
                        if fully_invalidated(Some(p), invalid_objects, unions) {
                            continue;
                        }
                        queue.push(p);
                    }
                    Some(p @ Pointer::Fn(f)) => {
                        queue.push(p);
                        if fn_assignments.get(&id) == Some(&f) {
                            continue;
                        }
                    }
                    Some(Pointer::NullOrVoid) => {}
                    None => continue,
                }
                var_assignments.insert(id, Assignment { rhs: arg });
            }
        }
        CallArgs::Invalid(none_count) => {
            for id in static_fn_data[func].param_indices().skip(*none_count) {
                if always_invalid_vars.contains(&id) {
                    continue;
                }
                var_assignments.insert(
                    id,
                    Assignment {
                        rhs: Some(Pointer::NullOrVoid),
                    },
                );
            }
        }
    }

    // TODO: eagerly check for RESOLVING_CALL e.g. when pushing, not popping, from queue.
    // This we we bail sooner.

    while let Some(o) = queue.pop() {
        match o {
            Pointer::Object(o) => {
                if o == ObjectStore::RESOLVING_CALL {
                    return Err(());
                }

                if !done_objects.insert(o) {
                    continue;
                }

                if invalid_objects.contains(o) {
                    continue;
                }

                for (key, value) in call_site_state
                    .prop_assignments
                    .range((o, NameId::from_u32(0))..(o, NameId::MAX))
                {
                    if depends_on_unresolved_call(value.rhs, unions) {
                        return Err(());
                    }

                    let new = if fully_invalidated(value.rhs, invalid_objects, unions) {
                        Assignment { rhs: None }
                    } else {
                        *value
                    };
                    if let Some(accessed_props) = accessed_props.get(&func) {
                        if accessed_props.contains(&key.1) {
                            prop_assignments.insert(*key, new);
                        }
                    } else {
                        prop_assignments.insert(*key, new);
                    }

                    match functions[func].entry_state.prop_assignments.entry(*key) {
                        std::collections::btree_map::Entry::Occupied(mut entry) => {
                            let union =
                                create_union(unions, entry.get().rhs, new.rhs, invalid_objects);
                            entry.insert(Assignment { rhs: union });
                        }
                        std::collections::btree_map::Entry::Vacant(entry) => {
                            entry.insert(new);
                        }
                    }

                    match new.rhs {
                        Some(Pointer::Fn(f)) => {
                            if !done_functions.contains(f) {
                                queue.push(Pointer::Fn(f));
                            }
                        }
                        Some(Pointer::Object(o)) => {
                            if !done_objects.contains(o) {
                                queue.push(Pointer::Object(o));
                            }
                        }
                        Some(Pointer::Union(u)) => {
                            for constituent in unions[u].constituents() {
                                if !done_objects.contains(constituent) {
                                    queue.push(Pointer::Object(constituent));
                                }
                            }
                        }
                        Some(Pointer::NullOrVoid) | None => {}
                    }
                }
            }
            Pointer::Union(union) => {
                for constituent in unions[union].constituents() {
                    if !done_objects.contains(constituent) {
                        queue.push(Pointer::Object(constituent));
                    }
                }
            }
            Pointer::Fn(f) => {
                if !done_functions.insert(f) {
                    continue;
                }

                for var in &static_fn_data[f].captured_vars {
                    if !done_vars.insert(*var) {
                        continue;
                    }
                    debug_assert!(!always_invalid_vars.contains(var));
                    let mut from_fn_assignments = false;
                    let value =
                        match call_site_state
                            .var_assignments
                            .get(var)
                            .copied()
                            .or_else(|| {
                                from_fn_assignments = true;
                                fn_assignments.get(var).map(|f| Assignment {
                                    rhs: Some(Pointer::Fn(*f)),
                                })
                            }) {
                            Some(v) => v,
                            None => continue,
                        };

                    if depends_on_unresolved_call(value.rhs, unions) {
                        return Err(());
                    }
                    let new = if fully_invalidated(value.rhs, invalid_objects, unions) {
                        Assignment { rhs: None }
                    } else {
                        value
                    };
                    match new.rhs {
                        Some(Pointer::Fn(f)) => {
                            if !done_functions.contains(f) {
                                queue.push(Pointer::Fn(f));
                            }
                            if from_fn_assignments {
                                continue;
                            }
                        }
                        Some(Pointer::Object(o)) => {
                            if !done_objects.contains(o) {
                                queue.push(Pointer::Object(o));
                            }
                        }
                        Some(Pointer::Union(u)) => {
                            for constituent in unions[u].constituents() {
                                if !done_objects.contains(constituent) {
                                    queue.push(Pointer::Object(constituent));
                                }
                            }
                        }
                        Some(Pointer::NullOrVoid) => {}
                        None => continue,
                    }
                    var_assignments.insert(*var, new);
                }
            }
            Pointer::NullOrVoid => unreachable!(),
        }
    }

    {
        let mut counts = DONE_VAR_COUNT.lock().unwrap();
        let count = done_vars.len();
        if count >= counts.len() {
            counts.resize(count + 1, 0);
            counts[count] += 1;
        }
    }

    debug_assert!(var_assignments.iter().all(|(k, v)| Some(*v)
        != fn_assignments.get(k).map(|f| Assignment {
            rhs: Some(Pointer::Fn(*f)),
        })));

    let call = Call {
        func,
        state: Lattice {
            var_assignments,
            prop_assignments,
        },
    };

    if cfg!(debug_assertions) {
        debug_assert!(!call
            .state
            .prop_assignments
            .keys()
            .any(|(o, _)| *o == ObjectStore::RESOLVING_CALL));
        debug_assert!(!call
            .state
            .prop_assignments
            .values()
            .any(|p| depends_on_unresolved_call(p.rhs, unions)));
        debug_assert!(!call
            .state
            .var_assignments
            .values()
            .any(|p| depends_on_unresolved_call(p.rhs, unions)));
    }

    Ok(call)
}

impl Annotation for Lattice {}

pub struct Renamer<'a> {
    pub program_data: &'a mut ProgramData,
    pub rename_map: FxHashMap<NodeId, JsWord>,
}

// TODO: node id's are unique. use rename_map.remove to get owned JsWord - no other node will access the entry anyway.
impl VisitMut<'_> for Renamer<'_> {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        if let Some(new_name) = self.rename_map.get(&node.node_id) {
            node.sym = new_name.clone();
        }
    }

    fn visit_mut_prop_name(&mut self, node: &mut PropName) {
        let node_id_to_rename = match node {
            // Handled by visit_mut_ident
            PropName::Ident(_) => None,
            PropName::Str(p) => Some(p.node_id),
            PropName::Num(p) => Some(p.node_id),
            PropName::Computed(p) => Some(p.expr.node_id()),
        };
        if let Some(node_id_to_rename) = node_id_to_rename {
            if let Some(new_name) = self.rename_map.get(&node_id_to_rename) {
                *node = PropName::Ident(Ident {
                    node_id: self.program_data.new_id_from(node.node_id()),
                    sym: new_name.clone(),
                    ctxt: SyntaxContext::empty(),
                });
                return;
            }
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_member_expr(&mut self, node: &mut MemberExpr) {
        if node.computed {
            if let Some(new_name) = self.rename_map.get(&node.prop.node_id()) {
                *node.prop.as_mut() = Expr::Ident(Ident {
                    node_id: self.program_data.new_id_from(node.prop.node_id()),
                    sym: new_name.clone(),
                    ctxt: SyntaxContext::empty(),
                });
                node.computed = false;
                return;
            }
        }
        node.visit_mut_children_with(self);
    }
}
