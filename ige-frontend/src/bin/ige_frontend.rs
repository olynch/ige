#[macro_use]
extern crate ige_frontend;
extern crate petgraph;
extern crate rmp_serde;
extern crate argparse;
extern crate serde;

use std::sync::{Arc, RwLock};
use std::sync::mpsc;

use std::os::unix::net::UnixStream;
use std::thread;

use argparse::*;
use serde::ser::Serialize;

use petgraph::graph::Graph;

use ige_frontend::display;
use ige_frontend::layout;
use ige_frontend::logic;
use ige_frontend::display::DisplayInput;
use ige_frontend::messages::*;

fn main() {
    let mut socket_name = String::new();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Frontend for IGE.");
        ap.refer(&mut socket_name)
            .add_argument("SOCKET_NAME", Store, "path to the socket");
        ap.parse_args_or_exit();
    }

    let write_stream = UnixStream::connect(&socket_name).unwrap();
    let read_stream = write_stream.try_clone().unwrap();

    let (tx_display_commands, rx_display_commands) = mpsc::channel();
    let (tx_events, rx_events) = mpsc::channel();
    let (tx_graph_commands, rx_graph_commands) = mpsc::channel();

    thread::spawn(clone!(tx_display_commands => move || {
        let mut controlmsg_stream = rmp_serde::decode::Deserializer::from_read(read_stream);
        loop {
            let msg: ControlMsg = serde::Deserialize::deserialize(&mut controlmsg_stream).unwrap();
            match msg {
                ControlMsg::DispMsg(cmd) => {
                    tx_display_commands.send(cmd).unwrap();
                }
                ControlMsg::GraphMsg(cmd) => {
                    tx_graph_commands.send(cmd).unwrap();
                }
            }
        }
    }));

    thread::spawn(move || {
        let mut event_stream = rmp_serde::encode::Serializer::new(write_stream);
        loop {
            let event: Event = rx_events.recv().unwrap();
            event.serialize(&mut event_stream).unwrap();
        }
    });

    let display_input = Arc::new(RwLock::new(DisplayInput::new()));

    let graph: Arc<RwLock<Graph<(),()>>> = Arc::new(RwLock::new(Graph::new()));

    {
        let mut g = graph.write().unwrap();
        let idx1 = g.add_node(());
        let idx2 = g.add_node(());
        g.add_edge(idx1, idx2, ());
    }

    let (tx_processing_to_layout, rx_processing_to_layout) = mpsc::channel();

    thread::spawn(clone!(graph => move || {
        logic::handle_input(graph.clone(), rx_graph_commands, tx_processing_to_layout);
    }));

    thread::spawn(clone!(display_input, graph => move || {
        layout::run_layout_engine(
            graph.clone(),
            display_input.clone(),
            rx_processing_to_layout,
            tx_display_commands);
    }));

    let window_thread = thread::spawn(clone!(display_input => move || {
        display::main_window(display_input.clone(), rx_display_commands, tx_events);
    }));

    window_thread.join().unwrap();
}
