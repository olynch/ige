extern crate ige_frontend;
extern crate petgraph;
extern crate rmp_serde;

use petgraph::graph::{Graph, NodeIndex};

use std::sync::{Arc, RwLock};
use std::sync::mpsc;
use std::thread;
use std::str::FromStr;

use std::os::unix::net::UnixStream;

use argparse::*;
use rmp_serde;
use serde;

use ige_frontend::display;
use ige_frontend::display::DisplayInput;
use ige_frontend::layout;
use ige_frontend::messages;

fn main() {
    let mut socket_name = String::new();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Frontend for IGE.");
        ap.refer(&mut socket_name)
            .add_argument("SOCKET_NAME", Store, "path to the socket");
        ap.parse_args_or_exit();
    }

    //let g = Arc::new(RwLock::new(Graph::new()));
    //let display_input = Arc::new(RwLock::new(DisplayInput { shapes: vec![], selectors: vec![] }));
    //let (tx_main_to_layout, rx_main_to_layout) = mpsc::channel();
    //let (tx_layout_to_display, rx_layout_to_display) = mpsc::channel();
    //let (tx_display_to_main, rx_display_to_main) = mpsc::channel();
    //let display_input_window = display_input.clone();
    //let window = thread::spawn(move || {
    //    display::main_window(display_input_window.clone(), rx_layout_to_display, tx_display_to_main)
    //});
    //let g_layout = g.clone();
    //let display_input_layout = display_input.clone();
    //thread::spawn(move || {
    //    layout::layout_thread(g_layout.clone(), display_input_layout.clone(), rx_main_to_layout, tx_layout_to_display)
    //});

    //let mut write_stream = UnixStream::connect(&socket_name).unwrap();
    //let mut read_stream = write_stream.try_clone().unwrap();


    thread::spawn(move || {
        let mut command_stream = rmp_serde::decode::Deserializer::from_read(read_stream);
        loop {
            let cmd: messages::Command = serde::Deserialize::deserialize(command_stream);
            println!("{:?}", cmd);
        }
    });

    thread::spawn(move || {
        loop {

}
