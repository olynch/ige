extern crate ige_frontend;
//extern crate petgraph;
extern crate rmp_serde;
extern crate argparse;
extern crate serde;

//use petgraph::graph::{Graph, NodeIndex};

use std::sync::{Arc, RwLock};
use std::sync::mpsc;
use std::{thread, time};

use std::os::unix::net::UnixStream;

use argparse::*;
//use rmp_serde;
use serde::ser::Serialize;

//use ige_frontend::display;
//use ige_frontend::display::DisplayInput;
//use ige_frontend::layout;
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

    let write_stream = UnixStream::connect(&socket_name).unwrap();
    let read_stream = write_stream.try_clone().unwrap();

    let (tx_sock_incoming, rx_sock_incoming) = mpsc::channel();
    let (tx_sock_outgoing, rx_sock_outgoing) = mpsc::channel();

    thread::spawn(move || {
        let mut controlmsg_stream = rmp_serde::decode::Deserializer::from_read(read_stream);
        loop {
            let cmd: ControlMsg = serde::Deserialize::deserialize(&mut controlmsg_stream).unwrap();
            tx_sock_incoming.send(cmd).unwrap();
        }
    });

    thread::spawn(move || {
        let mut event_stream = rmp_serde::encode::Serializer::new(write_stream);
        loop {
            let event: Event = rx_sock_outgoing.recv().unwrap();
            event.serialize(&mut event_stream).unwrap();
        }
    });

    thread::spawn(move || {
        loop {
            let cmd: ControlMsg = rx_sock_incoming.recv().unwrap();
            println!("{:?}", cmd);
        }
    });

    thread::spawn(move || {
        let events = vec![Event::KeyPress { key: 0, modifier: 1 }, Event::Selection { node_id: 5 }];
        loop {
            for event in &events {
                tx_sock_outgoing.send(*event).unwrap();
                thread::sleep(time::Duration::from_secs(2));
            }
        }
    });
}
