use super::{
    ColorGraphNode::ColorGraphNodeId,
    PropertyClustering::{PropertyClustering, PropertyClusteringId},
};
use ast::NodeId;
use ecma_visit::{noop_visit_mut_type, VisitMut, VisitMutWith};
use index::vec::IndexVec;
use rustc_hash::FxHashMap;
use swc_atoms::JsWord;

pub fn rename_use_sites(
    ast: &mut ::ast::Program,
    propertyClusterings: IndexVec<PropertyClusteringId, PropertyClustering>,
) {
    let mut renaming_map = FxHashMap::default();
    for mut prop in propertyClusterings {
        if prop.isInvalidated() {
            continue;
            // dbg!(prop);
            // todo!();
            //   this.renamingIndex.put(prop.getName(), INVALIDATED_NAME_VALUE);
            //   return;
        }

        let clusterNames = createAllClusterNames(&mut prop);

        if clusterNames.len() <= 1 {
            /**
             * Don't bother renaming clusters with a single element. Renaming won't actaully disambiguate
             * anything in this case, so skip the work.
             */
            //   self.renamingIndex.put(prop.getName(), prop.getName());
            continue;
        }

        // this.renamingIndex.putAll(prop.getName(), clusterNames.values());
        for (site, node) in prop.useSites {
            let flatRep = prop.clusters.find(node);
            let newName = clusterNames.get(&flatRep).unwrap();
            renaming_map.insert(site, newName.clone());
            // dbg!(&prop.name, newName);
            //   if newName != site.getString() {
            //     site.setString(newName);
            //     this.mutationCb.accept(site);
            //   }
        }
    }

    let mut renamer = UseSiteRenamer { renaming_map };

    ast.visit_mut_with(&mut renamer);
}

pub struct UseSiteRenamer {
    renaming_map: FxHashMap<NodeId, JsWord>,
}

// impl UseSiteRenamer {

//     /**
//      * Renames all references to {@code prop}.
//      *
//      * <p>If {@code prop} is invalid or should otherwise not be renamed, the AST will not be changed.
//      */
//     pub fn renameUses(&mut self, prop: &mut PropertyClustering) {
//         if prop.isInvalidated() {
//             todo!();
//             //   this.renamingIndex.put(prop.getName(), INVALIDATED_NAME_VALUE);
//             //   return;
//         }

//         let clusterNames = createAllClusterNames(prop);

//         if clusterNames.len() <= 1 {
//             /**
//              * Don't bother renaming clusters with a single element. Renaming won't actaully disambiguate
//              * anything in this case, so skip the work.
//              */
//             //   self.renamingIndex.put(prop.getName(), prop.getName());
//             return;
//         }

//         // this.renamingIndex.putAll(prop.getName(), clusterNames.values());
//         for (site, &node) in &prop.useSites {
//             let flatRep = prop.clusters.find(node);
//             let newName = clusterNames.get(&flatRep).unwrap();
//             dbg!(&prop.name, newName);
//             //   if newName != site.getString() {
//             //     site.setString(newName);
//             //     this.mutationCb.accept(site);
//             //   }
//         }
//     }
// }

/**
 * Creates a unique name for each cluster in {@code prop} and maps it to the cluster
 * representative.
 */
fn createAllClusterNames(prop: &mut PropertyClustering) -> FxHashMap<ColorGraphNodeId, JsWord> {
    prop.clusters
        .allRepresentatives()
        .into_iter()
        .map(|r| (r, createClusterName(prop, r)))
        .collect()
}

fn createClusterName(prop: &mut PropertyClustering, rep: ColorGraphNodeId) -> JsWord {
    if prop.getOriginalNameClusterRep() == Some(rep) {
        return prop.name.clone();
    }

    // TODO: maybe reserve cap
    let name = format_args!("JSC${}_{}", rep.as_u32(), prop.name).to_string();
    JsWord::from(name)
}

// TODO: since there is a one-to-one relation between idents and use sites,
// once we have renamed all id's we collected, we can abort the traversal
impl VisitMut for UseSiteRenamer {
    // TODO:
    // We only rename idents in types for testing, to make writing tests easier.
    // In normal builds there's no point in renaming these idents, so we skip the work.
    // #[cfg(not(test))]
    // noop_visit_mut_type!();

    fn visit_mut_ident(&mut self, ident: &mut ::ast::Ident) {
        if let Some(new_name) = self.renaming_map.get(&ident.node_id) {
            ident.sym = new_name.clone();
        }
    }
}
