extern crate petgraph;
extern crate ige_frontend;
extern crate regex;

use petgraph::graph::{Graph, NodeIndex};

use std::sync::{Arc, RwLock};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use std::str::FromStr;

use std::io;
use std::io::Write;

use regex::Regex;

use ige_frontend::display;
use ige_frontend::layout;

fn main() {
    let g = Arc::new(RwLock::new(Graph::new()));
    // {
    //     let mut graph = g.write().unwrap();
    //     let n1 = graph.add_node(());
    //     let n2 = graph.add_node(());
    //     let n3 = graph.add_node(());
    //     let n4 = graph.add_node(());
    //     // graph.add_edge(n1, n2, ());
    //     // graph.add_edge(n2, n3, ());
    //     // graph.add_edge(n3, n1, ());
    //     // graph.add_edge(n4, n2, ());
    // }
    let shapes = Arc::new(RwLock::new(vec![]));
    let (tx_main_to_layout, rx_main_to_layout) = mpsc::channel();
    let (tx_layout_to_display, rx_layout_to_display) = mpsc::channel();
    let (tx_display_to_main, rx_display_to_main) = mpsc::channel();
    let shapes_window = shapes.clone();
    let window = thread::spawn(move || {
        display::main_window(shapes_window.clone(), rx_layout_to_display, tx_display_to_main)
    });
    let g_layout = g.clone();
    thread::spawn(move || {
        layout::layout_thread(g_layout.clone(), shapes.clone(), rx_main_to_layout, tx_layout_to_display)
    });
    tx_main_to_layout.send(()).unwrap();
    let g_input = g.clone();
    thread::spawn(move || {
        // let add_node_re = Regex::new(r"^A (\d*\.\d*) (\d*.\d*)\n$").unwrap();
        let add_edge_re = Regex::new(r"^E (\d+) (\d+)\n$").unwrap();
        let del_node_re = Regex::new(r"^D (\d+)\n$").unwrap();
        loop {
            let mut input = String::new();
            print!("> ");
            io::stdout().flush();
            io::stdin().read_line(&mut input).unwrap();
            match input.chars().nth(0) {
                Some('A') => {
                    // match add_node_re.captures_iter(&input).nth(0) {
                    //     Some(cap) => {
                    //         let x = f64::from_str(&cap[1]).unwrap();
                    //         let y = f64::from_str(&cap[2]).unwrap();
                    //         let mut graph = g.write().unwrap();
                    //         let n = graph.add_node(display::Node::new(x, y));
                    //         tx_refresh.send(()).unwrap();
                    //         println!("Added node at index: {:?}", n);
                    //     }
                    //     None => {
                    //         println!("Failed to parse input");
                    //     }
                    // }
                    let mut graph = g_input.write().unwrap();
                    let n = graph.add_node(());
                    tx_main_to_layout.send(()).unwrap();
                    println!("Added node at index: {:?}", n);
                }
                Some('E') => {
                    match add_edge_re.captures_iter(&input).nth(0) {
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
                            println!("Failed to parse input");
                        }
                    }
                }
                Some('D') => {
                    match del_node_re.captures_iter(&input).nth(0) {
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
                            println!("Failed to parse input");
                        }
                    }
                }
                Some(_) => {}
                None => {}
            }
        }
    });
    thread::spawn(move || {
        loop {
            let ev = rx_display_to_main.recv().unwrap();
            match ev {
                display::KeyPress { key: k, modifier: m } => {
                    println!("Received keypress {} with modifier {:?}.", k, m);
                }
                display::Command { command: c } => {
                    println!("Received command {}.", c);
                }
            }
        }
    });
    window.join().unwrap();
}
