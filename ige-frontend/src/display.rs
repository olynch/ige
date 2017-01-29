use std::sync::{Arc, RwLock};
use std::rc::Rc;
use std::sync::mpsc::{Receiver, Sender};
use std::cell::Cell;
use std::char;

use std::f64::consts::PI;

use cgmath::{Point2, Vector2, Matrix2, EuclideanSpace};

use gtk;
use gtk::prelude::*;
use gtk::DrawingArea;

use cairo::{Context, FontFace};
use cairo::enums::{FontWeight, FontSlant};

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
                // println!("Displaying node at position {:?}", new_p);
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

impl DisplayData {
    fn translate(self, v: Vector2<f64>) -> Self {
        DisplayData {
            translation: self.translation + v,
            size_scaling: self.size_scaling,
            location_scaling: self.location_scaling
        }
    }
    fn zoom(self, s: f64) -> Self {
        DisplayData {
            translation: self.translation,
            size_scaling: self.size_scaling * s,
            location_scaling: self.location_scaling * s
        }
    }
    fn rotate(self, d: f64) -> Self {
        DisplayData {
            translation: self.translation,
            size_scaling: self.size_scaling,
            location_scaling: self.location_scaling * Matrix2::new(d.cos(), -d.sin(), d.sin(), d.cos())
        }
    }
}

#[derive(Copy, Clone)]
struct EditorState {
    mode: Mode
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
enum Mode {
    Backend,
    Viewer,
    Selector,
    Command
}

struct WindowState {
    display_data: Rc<Cell<DisplayData>>,
    editor_state: Rc<Cell<EditorState>>,
    command: Rc<RwLock<String>>,
    window: Rc<gtk::Window>,
    drawing_area: Rc<DrawingArea>
}

/// Runs the main window. Takes in an arc of a mutex of a graph to be rendered as well
/// as a channel where a message is sent whenever a the graph gets updated and the
/// graph should be re-rendered
pub fn main_window(shapes: Arc<RwLock<Vec<Shape>>>, rx: Receiver<()>, tx: Sender<Event>) -> () {
    if gtk::init().is_err() {
        panic!("Failed to initialize gtk");
    }

    let window_state = Rc::new(WindowState {
        display_data: Rc::new(Cell::new(DisplayData {
            translation: Vector2::new(50., 50.),
            size_scaling: 500.,
            location_scaling: Matrix2::new(500., 0., 0., 500.)
        })),
        window: Rc::new(gtk::Window::new(gtk::WindowType::Toplevel)),
        drawing_area: Rc::new(DrawingArea::new()),
        command: Rc::new(RwLock::new(String::new())),
        editor_state: Rc::new(Cell::new(EditorState {
            mode: Mode::Viewer
        }))
    });

    {
        let window_state_draw = window_state.clone();
        window_state.drawing_area.connect_draw(move |_, cr| {
            display(shapes.clone(), window_state_draw.clone(), cr);
            Inhibit(false)
        });
    }
    {
        let event_sender = tx.clone();
        let editor_state = window_state.editor_state.clone();
        let display_data = window_state.display_data.clone();
        let window_key = window_state.window.clone();
        let command = window_state.command.clone();
        let movement_keys: Vec<u32> = vec![u32::from('h'), u32::from('j'), u32::from('k'), u32::from('l')];
        let zoom_keys: Vec<u32> = vec![u32::from('+'), u32::from('-')];
        let rotate_keys: Vec<u32> = vec![u32::from('<'), u32::from('>')];
        window_state.window.connect_key_press_event(move |_, key| {
            let keyval = key.get_keyval();
            let keystate = key.get_state();

            // println!("key pressed: {} / {:?}", keyval, keystate);
            match editor_state.get().mode {
                Mode::Command => {
                    let mut command_str = command.write().unwrap();
                    if keyval == 0xff0d {
                        event_sender.send(Command { command: command_str.clone() }).unwrap();
                        println!("Sent command: {}", command_str.as_str());
                        command_str.clear();
                        editor_state.set(EditorState { mode: Mode::Viewer });
                        println!("Mode changed to Viewer");
                        window_key.queue_draw();
                    } else if keyval == 0xff1b {
                        command_str.clear();
                        editor_state.set(EditorState { mode: Mode::Viewer });
                        window_key.queue_draw();
                    } else if keyval == 0xff08 {
                        command_str.pop();
                        window_key.queue_draw();
                    }
                    else if keyval < 128 {
                        match char::from_u32(keyval) {
                            Some(c) => {
                                if !c.is_control() {
                                    command_str.push(c);
                                    window_key.queue_draw();
                                }
                            }
                            None => {}
                        }
                    }
                }
                Mode::Backend => {
                    event_sender.send(KeyPress { key: keyval, modifier: keystate }).unwrap();
                }
                Mode::Viewer => {
                    if movement_keys.contains(&keyval) {
                        let move_vec = match keyval {
                            104 => { Vector2::new(20., 0.) }
                            106 => { Vector2::new(0., -20.) }
                            107 => { Vector2::new(0., 20.) }
                            108 => { Vector2::new(-20., 0.) }
                            _ => { unreachable!() }
                        };
                        display_data.set(display_data.get().translate(move_vec));
                        window_key.queue_draw();
                    } else if zoom_keys.contains(&keyval) {
                        let zoom_fact = match keyval {
                            43 => { 1.5 }
                            45 => { 0.75 }
                            _ => { unreachable!() }
                        };
                        display_data.set(display_data.get().zoom(zoom_fact));
                        window_key.queue_draw();
                    } else if rotate_keys.contains(&keyval) {
                        let rotation = match keyval {
                            60 => { 0.25 * PI }
                            62 => { -0.25 * PI }
                            _ => { unreachable!() }
                        };
                        display_data.set(display_data.get().rotate(rotation));
                        window_key.queue_draw();
                    } else if keyval == u32::from(':') {
                        editor_state.set(EditorState { mode: Mode::Command });
                        println!("Mode changed to Command");
                        window_key.queue_draw();
                    } else if keyval == u32::from('q') {
                        gtk::main_quit();
                    }
                }
                Mode::Selector => {
                }
            }


            Inhibit(false)
        });
    }

    window_state.window.connect_delete_event(|_, _| {
        gtk::main_quit();
        Inhibit(false)
    });

    {
        let drawing_area = window_state.drawing_area.clone();
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

    window_state.window.add(&*window_state.drawing_area);
    window_state.window.show_all();
    gtk::main()
}


fn display(shapes: Arc<RwLock<Vec<Shape>>>,
           window_state: Rc<WindowState>,
           cr: &Context) {
    let shape_vec = shapes.read().unwrap();
    cr.set_source_rgb(0.1, 0.1, 0.1);
    cr.paint();
    cr.set_source_rgb(0.9, 0.9, 0.9);
    for s in shape_vec.iter() {
        s.draw(window_state.display_data.get(), cr);
    }
    match window_state.editor_state.get().mode{
        Mode::Command => {
            let command_str = window_state.command.read().unwrap();
            let mut displ_command = ":".to_string();
            displ_command.push_str(command_str.as_str());
            displ_command.push('\u{2588}');
            let disp_rect = window_state.drawing_area.get_allocation();
            let face = FontFace::toy_create("monospace", FontSlant::Normal, FontWeight::Normal);
            cr.set_font_face(face);
            cr.set_font_size(15.);
            cr.move_to(0., (disp_rect.height as f64) - 10.);
            cr.show_text(displ_command.as_str());
        }
        _ => {}
    }
}
