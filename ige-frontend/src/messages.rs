use serde::{Serialize, Deserialize};

// We should have some way of threading events and controlmsgs
// so that we can have responses to specific messages

/// The type of messages we send from Haskell to Rust
#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub enum ControlMsg {
    AddNode,
    DelNode {
        node_id: u32
    },
    AddEdge {
        node_id1: u32,
        node_id2: u32
    },
    DelEdge {
        edge_id: u32,
    },
    GetSelection,
    Zoom {
        percent: f64
    },
    Rotate {
        radians: f64
    },
    Translate {
        x: f64,
        y: f64
    }
}

/// The type of messages we send from Rust to Haskell
#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub enum Event {
    KeyPress {
        key: u32,
        modifier: u32
    },
    Selection {
        node_id: u32
    }
}
