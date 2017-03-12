use serde::{Serialize, Deserialize};

/// The type of messages we send from Haskell to Rust
#[derive(Serialize, Deserialize, Debug)]
pub enum Command {
    AddNode,
    AddEdge {
        node_id1: u32,
        node_id2: u32
    },
    DelNode {
        node_id: u32
    },
    BeginSelection
}

/// The type of messages we send from Rust to Haskell
#[derive(Serialize, Deserialize, Debug)]
pub enum Event {
    KeyPress {
        key: u32,
        modifier: u32
    },
    Command {
        cmd: String
    },
    Selection {
        node_id: u32
    }
}
