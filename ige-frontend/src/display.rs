use std::sync::{Arc, RwLock};
use std::rc::Rc;
use std::sync::mpsc::{Receiver, Sender};

use std::f64::consts::PI;

use cgmath::{Point2, Vector2, Matrix2, EuclideanSpace};

use gtk;
use gtk::prelude::*;
use gtk::DrawingArea;

use cairo::Context;

use gdk::enums::key::Key;
use gdk::ModifierType;

pub enum Event {
    KeyPress { key: Key, modifier: ModifierType },
    Command { command: String }
}

pub use self::Event::KeyPress;
pub use self::Event::Command;

pub enum Shape {
    Line { p1: Point2<f64>, p2: Point2<f64> },
    Dot { p: Point2<f64>, r: f64 }
}

impl Shape {
    fn draw(&self, display_data: DisplayData, cr: &Context) {
        match self {
            &Shape::Line { p1, p2 } => {
                let new_p1 = display_data.location_scaling * p1.to_vec() + display_data.translation;
                let new_p2 = display_data.location_scaling * p2.to_vec() + display_data.translation;
                cr.move_to(new_p1.x, new_p1.y);
                cr.line_to(new_p2.x, new_p2.y);
                cr.stroke()
            }
            &Shape::Dot { p, r } => {
                let new_p = display_data.location_scaling * p.to_vec() + display_data.translation;
                println!("Displaying node at position {:?}", new_p);
                cr.arc(new_p.x, new_p.y, display_data.size_scaling * r, 0.0, 2.0 * PI);
                cr.fill();
            }
        }
    }
}

#[derive(Copy, Clone)]
struct DisplayData {
    translation: Vector2<f64>,
    size_scaling: f64,
    location_scaling: Matrix2<f64>
}

/// Runs the main window. Takes in an arc of a mutex of a graph to be rendered as well
/// as a channel where a message is sent whenever a the graph gets updated and the
/// graph should be re-rendered
pub fn main_window(shapes: Arc<RwLock<Vec<Shape>>>, rx: Receiver<()>, tx: Sender<Event>) -> () {
    if gtk::init().is_err() {
        panic!("Failed to initialize gtk");
    }

    let window = gtk::Window::new(gtk::WindowType::Toplevel);
    let drawing_area = Rc::new(DrawingArea::new());
    let display_data = DisplayData {
        translation: Vector2::new(50., 50.),
        size_scaling: 500.,
        location_scaling: Matrix2::new(500., 0., 0., 500.)
    };
    drawing_area.connect_draw(move |_, cr| { display(shapes.clone(), display_data, cr); Inhibit(false) });
    let key_press_sender = tx.clone();
    window.connect_key_press_event(move |_, key| {
        let keyval = key.get_keyval();
        let keystate = key.get_state();

        println!("key pressed: {} / {:?}", keyval, keystate);
        key_press_sender.send(KeyPress { key: keyval, modifier: keystate }).unwrap();

        Inhibit(false)
    });

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

fn display(shapes: Arc<RwLock<Vec<Shape>>>, display_data: DisplayData, cr: &Context) {
    let shape_vec = shapes.read().unwrap();
    cr.set_source_rgb(0.1, 0.1, 0.1);
    cr.paint();
    cr.set_source_rgb(0.9, 0.9, 0.9);
    for s in shape_vec.iter() {
        s.draw(display_data, cr);
    }
}
