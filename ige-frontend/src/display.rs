use std::sync::{Arc, RwLock};
use std::rc::Rc;
use std::sync::mpsc::Receiver;

use std::f64::consts::PI;

use petgraph::graph::Graph;
use petgraph::visit::{IntoNodeReferences, EdgeRef};

use gtk;
use gtk::prelude::*;
use gtk::DrawingArea;

use glib;

use cairo::enums::{FontSlant, FontWeight};
use cairo::Context;

pub struct Node {
    tag: String,
    x: f64,
    y: f64,
}

impl Node {
    pub fn new(x: f64, y: f64, tag: String) -> Node {
        Node { x: x, y: y, tag: tag }
    }
}

/// Runs the main window. Takes in an arc of a mutex of a graph to be rendered as well
/// as a channel where a message is sent whenever a the graph gets updated and the
/// graph should be re-rendered
pub fn main_window(g: Arc<RwLock<Graph<Node, ()>>>, rx: Receiver<()>) -> () {
    if gtk::init().is_err() {
        panic!("Failed to initialize gtk");
    }

    let window = gtk::Window::new(gtk::WindowType::Toplevel);
    let drawing_area = Rc::new(DrawingArea::new());
    drawing_area.connect_draw(move |_, cr| { render(g.clone(), cr); Inhibit(false) });


    window.connect_delete_event(|_, _| {
        gtk::main_quit();
        Inhibit(false)
    });

    {
        let drawing_area = drawing_area.clone();
        gtk::timeout_add(50, move || {
            match rx.try_recv() {
                Ok(_) => {
                    drawing_area.queue_draw();
                }
                Err(_) => {}
            }
            gtk::Continue(true)
        });
    }

    window.add(&*drawing_area);
    window.show_all();
    gtk::main()
}

fn render(g: Arc<RwLock<Graph<Node, ()>>>, cr: &Context) {
    let graph = g.read().unwrap();
    cr.set_source_rgb(0.1, 0.1, 0.1);
    cr.paint();
    cr.set_source_rgb(0.9, 0.9, 0.9);
    for n in (*graph).node_references() {
        let (x, y) = (n.1.x, n.1.y);
        // let tag = &n.weight().tag;
        cr.arc(x, y, 3.0, 0.0, 2.0 * PI);
        cr.fill();
    }
    for e in (*graph).edge_references() {
        let (source_idx, target_idx) = (e.source(), e.target());
        let (source, target) = (&graph[source_idx], &graph[target_idx]);
        cr.move_to(source.x, source.y);
        cr.line_to(target.x, target.y);
        cr.stroke()
    }
}
