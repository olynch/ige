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

fn main() {
    let g = Arc::new(RwLock::new(Graph::new()));
    {
        let mut graph = g.write().unwrap();
        let n1 = graph.add_node(display::Node::new(200.0, 200.0, "Node 1".to_string()));
        let n2 = graph.add_node(display::Node::new(400.0, 400.0, "Node 2".to_string()));
        graph.add_edge(n1, n2, ());
    }
    let (tx, rx) = mpsc::channel();
    let sent_g = g.clone();
    let window = thread::spawn(move || {
        display::main_window(sent_g.clone(), rx)
    });
    // thread::sleep(Duration::new(1, 0));
    // {
    //     let mut graph = g.write().unwrap();
    //     let n1 = graph.add_node(display::Node::new(250.0, 200.0, "Node 1".to_string()));
    //     let n2 = graph.add_node(display::Node::new(450.0, 400.0, "Node 2".to_string()));
    //     graph.add_edge(n1, n2, ());
    //     tx.send(()).unwrap();
    // }
    let add_node_re = Regex::new(r"^A (\d*\.\d*) (\d*.\d*) (\w+)\n$").unwrap();
    let add_edge_re = Regex::new(r"^E (\d+) (\d+)\n$").unwrap();
    // let add_edge_re = Regex::
    loop {
        let mut input = String::new();
        print!("> ");
        io::stdout().flush();
        io::stdin().read_line(&mut input).unwrap();
        if input.chars().nth(0) == Some('q') {
            break;
        } else if input.chars().nth(0) == Some('A') {
            match add_node_re.captures_iter(&input).nth(0) {
                Some(cap) => {
                    let x = f64::from_str(&cap[1]).unwrap();
                    let y = f64::from_str(&cap[2]).unwrap();
                    let tag = cap[3].to_string();
                    let mut graph = g.write().unwrap();
                    let n = graph.add_node(display::Node::new(x, y, tag));
                    tx.send(()).unwrap();
                    println!("Added node at index: {:?}", n);
                }
                None => {
                    println!("Failed to parse input");
                }
            }
        } else if input.chars().nth(0) == Some('E') {
            match add_edge_re.captures_iter(&input).nth(0) {
                Some(cap) => {
                    let source = NodeIndex::new(usize::from_str(&cap[1]).unwrap());
                    let sink = NodeIndex::new(usize::from_str(&cap[2]).unwrap());
                    let mut graph = g.write().unwrap();
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
                    tx.send(()).unwrap();
                }
                None => {
                    println!("Failed to parse input");
                }
            }
        }
    }
    window.join().unwrap();
}
