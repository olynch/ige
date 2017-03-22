use petgraph::graph::{Graph, NodeIndex, EdgeIndex};

use std::sync::{Arc, RwLock};
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};

use messages::*;

pub fn handle_input(graph: Arc<RwLock<Graph<(),()>>>, rx: Receiver<GraphCommand>, tx: Sender<()>) {
    loop {
        let msg = rx.recv().unwrap();
        let mut g = graph.write().unwrap();
        match msg {
            GraphCommand::AddNode => {
                g.add_node(());
            }
            GraphCommand::DelNode { node_id } => {
                g.remove_node(NodeIndex::new(node_id as usize));
            }
            GraphCommand::AddEdge { node_id1, node_id2 } => {
                g.add_edge(NodeIndex::new(node_id1 as usize), NodeIndex::new(node_id2 as usize), ());
            }
            GraphCommand::DelEdge { edge_id } => {
                g.remove_edge(EdgeIndex::new(edge_id as usize));
            }
        }
        tx.send(()).unwrap();
    }
}
