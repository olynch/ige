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
    let g = Arc::new(RwLock::new(Graph::new()));
    {
        // let mut graph = g.write().unwrap();
        // let node_idxs: Vec<NodeIndex> = (0..9).map(|_| { graph.add_node(()) }).collect();
        // for i in 0..8 {
        //     for j in (i + 1)..9 {
        //         graph.add_edge(node_idxs[i], node_idxs[j], ());
        //     }
        // }
        // graph.add_edge(n1, n2, ());
        // graph.add_edge(n2, n3, ());
        // graph.add_edge(n3, n1, ());
        // graph.add_edge(n4, n2, ());
    }
    let display_input = Arc::new(RwLock::new(DisplayInput { shapes: vec![], selectors: vec![] }));
    let (tx_main_to_layout, rx_main_to_layout) = mpsc::channel();
    let (tx_layout_to_display, rx_layout_to_display) = mpsc::channel();
    let (tx_display_to_main, rx_display_to_main) = mpsc::channel();
    let display_input_window = display_input.clone();
    let window = thread::spawn(move || {
        display::main_window(display_input_window.clone(), rx_layout_to_display, tx_display_to_main)
    });
    let g_layout = g.clone();
    let display_input_layout = display_input.clone();
    thread::spawn(move || {
        layout::layout_thread(g_layout.clone(), display_input_layout.clone(), rx_main_to_layout, tx_layout_to_display)
    });
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
    window.join().unwrap();
}
