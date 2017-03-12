extern crate pipes;
extern crate serde_json;
extern crate argparse;

use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::{OpenOptions, File};
use pipes::*;
use argparse::*;

fn main() {
    let mut command_fifo_name = String::new();
    let mut event_fifo_name = String::new();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Test named pipes.");
        ap.refer(&mut command_fifo_name)
            .add_argument("<command_fifo>", Store, "Fifo to accept commands through");
        ap.refer(&mut event_fifo_name)
            .add_argument("<event_fifo>", Store, "Fifo to send events through");
        ap.parse_args_or_exit();
    }

    let commands = File::open(&command_fifo_name).unwrap();
    let mut commands_reader = BufReader::new(commands);

    let mut events = OpenOptions::new().append(true).open(&event_fifo_name).unwrap();

    let mut cmd_str = String::new();
    commands_reader.read_line(&mut cmd_str).unwrap();

    let deserialized: Command = serde_json::from_str(&cmd_str).unwrap();
    println!("deserialized = {:?}", deserialized);

    let serialized = serde_json::to_string(&deserialized).unwrap();
    write!(&mut events, "{}\n", &serialized).unwrap();
}
