//! This file contains lots of stuff that should really be refactored
//! into a library. Currently, it runs the entire editor.
//! TODO: Refactor the important bits into relevant modules
//! TODO: Replace with ige_frontend, a binary that connects to a socket
//! and sends events and reads commands from that socket.

extern crate petgraph;
extern crate ige_frontend;
extern crate regex;

use petgraph::graph::{Graph, NodeIndex};

use std::sync::{Arc, RwLock};
use std::sync::mpsc;
use std::thread;
use std::str::FromStr;

use regex::Regex;

use ige_frontend::display;
use ige_frontend::display::DisplayInput;
use ige_frontend::layout;

fn main() {
    // This is the graph that we are editing, also the communication data between the
    // logic thread and the layout thread
    let g = Arc::new(RwLock::new(Graph::new()));

    // This is the initial contents of the window in vector form, also the communication
    // data between the layout thread and the display thread
    let display_input = Arc::new(RwLock::new(DisplayInput { shapes: vec![], selectors: vec![] }));

    // Names tell it all
    let (tx_main_to_layout, rx_main_to_layout) = mpsc::channel();
    let (tx_layout_to_display, rx_layout_to_display) = mpsc::channel();
    let (tx_display_to_main, rx_display_to_main) = mpsc::channel();

    // Display thread
    let display_input_window = display_input.clone();
    let window = thread::spawn(move || {
        display::main_window(display_input_window.clone(), rx_layout_to_display, tx_display_to_main)
    });

    // Layout thread
    let g_layout = g.clone();
    let display_input_layout = display_input.clone();
    thread::spawn(move || {
        layout::layout_thread(g_layout.clone(), display_input_layout.clone(), rx_main_to_layout, tx_layout_to_display)
    });

    // Logic(/main) thread
    tx_main_to_layout.send(()).unwrap();
    let g_input = g.clone();
    thread::spawn(move || {
        let add_edge_re = Regex::new(r"^E (\d+) (\d+)$").unwrap();
        let del_node_re = Regex::new(r"^D (\d+)$").unwrap();
        loop {
            let ev = rx_display_to_main.recv().unwrap();
            match ev {
                display::KeyPress { key: k, modifier: m } => {
                    println!("Received keypress {} with modifier {:?}.", k, m);
                }
                display::Selection { key: i } => {
                    println!("Recieved selection {}.", i);
                }
                display::Command { command: c } => {
                    println!("Received command {}.", c);
                    match c.chars().nth(0) {
                        Some('A') => {
                            let mut graph = g_input.write().unwrap();
                            let n = graph.add_node(());
                            tx_main_to_layout.send(()).unwrap();
                            println!("Added node at index: {:?}", n);
                        }
                        Some('E') => {
                            match add_edge_re.captures_iter(&c).nth(0) {
                                Some(cap) => {
                                    let source = NodeIndex::new(usize::from_str(&cap[1]).unwrap());
                                    let sink = NodeIndex::new(usize::from_str(&cap[2]).unwrap());
                                    let mut graph = g_input.write().unwrap();
                                    match graph.node_weight(source) {
                                        Some(_) => {}
                                        None => {
                                            println!("Source node does not exist.");
                                            continue;
                                        }
                                    }
                                    match graph.node_weight(sink) {
                                        Some(_) => {}
                                        None => {
                                            println!("Sink node does not exist");
                                            continue;
                                        }
                                    }
                                    graph.add_edge(source, sink, ());
                                    tx_main_to_layout.send(()).unwrap();
                                }
                                None => {
                                    println!("Failed to parse command");
                                }
                            }
                        }
                        Some('D') => {
                            match del_node_re.captures_iter(&c).nth(0) {
                                Some(cap) => {
                                    let idx = NodeIndex::new(usize::from_str(&cap[1]).unwrap());
                                    let mut graph = g_input.write().unwrap();
                                    match graph.remove_node(idx) {
                                        Some(_) => {}
                                        None => {
                                            println!("Node does not exist");
                                            continue;
                                        }
                                    }
                                    tx_main_to_layout.send(()).unwrap();
                                }
                                None => {
                                    println!("Failed to parse command");
                                }
                            }
                        }
                        Some(_) => {}
                        None => {}
                    }
                }
            }
        }
    });

    // wait until we quit the gtk window
    window.join().unwrap();
}
