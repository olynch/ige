#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate serde;
extern crate rmp_serde;

use rmp_serde::{from_slice, to_vec, Deserializer, Serializer};
use serde::{Serialize, Deserialize};

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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        // let event = Command::AddEdge { node_id1: 1, node_id2: 2 };
        let event = Command::AddNode;

        // let serialized = serde_json::to_string(&event).unwrap();
        let buf = to_vec(&event).unwrap();
        println!("serialized = {:?}", buf);

        let deserialized: Command = from_slice(&buf[..]).unwrap();
        println!("deserialized = {:?}", deserialized);

        assert!(false);
    }
}
