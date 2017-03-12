extern crate argparse;

use std::os::unix::net::UnixStream;
use std::io::prelude::*;
use argparse::*;
use std::io;

fn main() {
    let mut socket_name = String::new();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Test sockets.");
        ap.refer(&mut socket_name)
            .add_argument("<socket_name>", Store, "path to the socket");
        ap.parse_args_or_exit();
    }
    let mut stream = UnixStream::connect(&socket_name).unwrap();
    // println!("response_length: {}", response.len());
    loop {
        let mut response: Vec<u8> = (0..100).map(|_| 0).collect();
        let mut input = String::new();
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();
        input.pop();
        stream.write_all(input.as_bytes()).unwrap();
        let n = stream.read(&mut response[..]).unwrap();
        println!("response: {}, length: {}", String::from_utf8(response.clone()).unwrap(), n);
    };
}
