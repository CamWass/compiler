use super::ColorGraphBuilder::EdgeReason;
use super::ColorGraphNode::{ColorGraphNodeId, PropAssociation};
use super::ColorGraphNodeFactory::ColorGraphNodeFactory;
use super::PropertyClustering::*;
use crate::graph::FixedPointGraphTraversal::EdgeCallback;
use index::vec::IndexVec;

pub struct ClusterPropagator<'a, 'col> {
    nodefactory: &'a mut ColorGraphNodeFactory<'col>,
    propertyClusterings: &'a mut IndexVec<PropertyClusteringId, PropertyClustering>,
}

impl<'a, 'col> ClusterPropagator<'a, 'col> {
    pub fn new(
        nodefactory: &'a mut ColorGraphNodeFactory<'col>,
        propertyClusterings: &'a mut IndexVec<PropertyClusteringId, PropertyClustering>,
    ) -> Self {
        Self {
            nodefactory,
            propertyClusterings,
        }
    }
}

impl EdgeCallback<ColorGraphNodeId, EdgeReason> for ClusterPropagator<'_, '_> {
    fn traverseEdge(
        &mut self,
        source: &ColorGraphNodeId,
        e: &EdgeReason,
        destination: &ColorGraphNodeId,
    ) -> bool {
        let startDestPropCount = self.nodefactory.nodes[*destination].associatedProps.len();

        // TODO: bad collect:
        let source_props = self.nodefactory.nodes[*source]
            .associatedProps
            .keys()
            .copied()
            .collect::<Vec<_>>();
        for prop in source_props {
            if self.propertyClusterings[prop].isInvalidated() {
                continue;
            }

            self.nodefactory.nodes[*destination]
                .associatedProps
                .entry(prop)
                .or_insert(PropAssociation::SUPERTYPE);
            self.propertyClusterings[prop]
                .clusters
                .union(*source, *destination);
        }

        // Were any properties added to dest?
        startDestPropCount < self.nodefactory.nodes[*destination].associatedProps.len()
    }
}
