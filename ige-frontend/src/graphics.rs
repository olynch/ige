extern crate libc;

use std::ptr;

struct X11Surface {
    cairo_surface: 
}

trait DrawingSurface {
    type TL: TextLayout;

    fn size(&self) -> (i32, i32);
    fn move_to(&mut self, p: (i32, i32)) -> ();
    fn line_to(&mut self, p: (i32, i32)) -> ();
    fn text_at(&mut self, p: (i32, i32), text: TL) -> ();
    fn dot_at(&mut self, p: (i32, i32), radius: i32) -> ();
    fn flush(&mut self) -> ();
}

trait TextLayout {
    fn size(&self) -> (i32, i32);
}
