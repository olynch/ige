//! This module contains the apparatus for computing the layout of a graph

use std::sync::{Arc, RwLock};
use std::sync::mpsc::{Receiver, Sender};

use std::f64;

use petgraph::graph::{Graph, Frozen};
use petgraph::visit::{EdgeRef, IntoNodeIdentifiers, IntoNodeReferences};

use cgmath::{Point2, Vector2};
use cgmath::InnerSpace;

use display::Shape;

enum LEdge {
    Spring { k: f64, l: f64 }
}

const DOT_RADIUS: f64 = 0.01;
const STABLE_MOVEMENT: f64 = 0.01;
const STEP_SIZE: f64 = 0.01;
const DEFAULT_K: f64 = 0.1;
const DEFAULT_L: f64 = 0.4;

pub fn layout_thread<N, E>(source_graph: Arc<RwLock<Graph<N, E>>>,
                       shapes: Arc<RwLock<Vec<Shape>>>,
                       rx: Receiver<()>,
                       tx: Sender<()>) -> () {
    loop {
        let _ = rx.recv().unwrap();
        let mut shapes = shapes.write().unwrap();
        let source_graph = source_graph.read().unwrap();
        let mut layout_graph = initialize_layout(&source_graph);
        iter_fd_step(&mut Frozen::new(&mut layout_graph));
        render_layout(&layout_graph, &mut shapes);
        tx.send(()).unwrap();
    }
}

fn initialize_layout<N, E>(source_graph: &Graph<N, E>)
                           -> Graph<Point2<f64>, LEdge> {
    // To start off, I'm just going to put the nodes in a square.
    // Figuring out what the initial configuration ought to look like is difficult problem
    let n_nodes = source_graph.node_count();
    let sqr_dim = (n_nodes as f64).sqrt().ceil() as i32;
    let spacing = 1. / (sqr_dim as f64);
    let mut layout_graph = source_graph.filter_map(
        |i, _| {
            let x = ((i.index() as i32 % sqr_dim) as f64) * spacing;
            let y = ((i.index() as i32 / sqr_dim) as f64) * spacing;
            Some(Point2::new(x, y))
        },
        |_, _| {
            None
        });
    for e in source_graph.edge_references() {
        layout_graph.add_edge(e.target(), e.source(), LEdge::Spring { k: DEFAULT_K, l: DEFAULT_L });
        layout_graph.add_edge(e.source(), e.target(), LEdge::Spring { k: DEFAULT_K, l: DEFAULT_L });
    }
    layout_graph
}

fn iter_fd_step(layout_graph: &mut Frozen<Graph<Point2<f64>, LEdge>>) -> () {
    while fd_step(layout_graph, STEP_SIZE) > STABLE_MOVEMENT {}
}

fn render_layout(layout_graph: &Graph<Point2<f64>, LEdge>,
                 shapes: &mut Vec<Shape>) -> () {
    shapes.clear();
    for n in layout_graph.node_references() {
        shapes.push(Shape::Dot { p: *n.1, r: DOT_RADIUS})
    }
    for e in layout_graph.edge_references() {
        let p1 = layout_graph[e.source()];
        let p2 = layout_graph[e.target()];
        shapes.push(Shape::Line { p1: p1, p2: p2 })
    }
}

/// Force directed step: simulate one frame of movement, return the max
/// distance moved.
fn fd_step(g: &mut Frozen<Graph<Point2<f64>, LEdge>>, dt: f64) -> f64 {
    let mut max_dl = 0.0;
    for id in g.node_identifiers() {
        let mut force = Vector2::new(0., 0.);
        let p1 = g[id];
        for e in g.edges(id) {
            match e.weight() {
                &LEdge::Spring {k, l} => {
                    let p2 = g[e.target()];
                    let v = p2 - p1;
                    let d = v.magnitude();
                    let dl = d - l;
                    max_dl = if dl > max_dl { dl } else { max_dl };
                    force += k * dl * (v / d);
                }
            }
        }
        g[id] += force * dt;
    }
    max_dl
}
