struct X11Surface {
    
}

trait DrawingSurface {
    type Ctx: DrawingContext;

    fn get_context(&mut self) -> Ctx;
}

trait DrawingContext {
    type TL: TextLayout;

    fn size(&self) -> (i32, i32);
    fn move_to(&mut self, p: (i32, i32)) -> ();
    fn line_to(&mut self, p: (i32, i32)) -> ();
    fn text_at(&mut self, p: (i32, i32), text: TL) -> ();
    fn dot_at(&mut self, p: (i32, i32), radius: i32) -> ();
}

trait TextLayout {
    fn size(&self) -> (i32, i32);
}
