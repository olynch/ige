#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <cairo.h>
#include <cairo-xlib.h>

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <pango/pangocairo.h>
#include "graphs.h"

/*! Simple Cairo/Xlib example.
 * @author Bernhard R. Fischer, 2048R/5C5FFD47 <bf@abenteuerland.at>.
 * @version 2014110801
 * Compile with
 * gcc -Wall $(pkg-config --libs --cflags cairo x11) -o cairo_xlib_simple cairo_xlib_simple.c
 */

static void
draw_text (cairo_t *cr, char * text, int x, int y)
{
/* #define RADIUS 150 */
/* #define N_WORDS 10 */
#define FONT "Hack 14"

  PangoLayout *layout;
  PangoFontDescription *desc;

  /* Center coordinates on the middle of the region we are drawing
   */
  /* cairo_translate (cr, RADIUS, RADIUS); */

  /* Create a PangoLayout, set the font and text */
  layout = pango_cairo_create_layout (cr);

  pango_layout_set_text (layout, text, -1);
  desc = pango_font_description_from_string (FONT);
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  /* Draw the layout N_WORDS times in a circle */
  /* for (i = 0; i < N_WORDS; i++) */
  /*   { */
  /* int i = 0; */
  int width, height;
  /* double angle = (360. * i) / N_WORDS; */
  /* double red; */

  cairo_save (cr);

  /* Gradient from red at angle == 60 to blue at angle == 240 */
  /* red   = (1 + cos ((angle - 60) * G_PI / 180.)) / 2; */
  cairo_set_source_rgb (cr, 0.0, 0.0, 0.0);

  /* cairo_rotate (cr, angle * G_PI / 180.); */

  /* Inform Pango to re-layout the text with the new transformation */
  /* pango_cairo_update_layout (cr, layout); */

  pango_layout_get_size (layout, &width, &height);
  cairo_move_to (cr, x - ((double)width / PANGO_SCALE) / 2, y - ((double)height / PANGO_SCALE) / 2);
  pango_cairo_show_layout (cr, layout);

  cairo_restore (cr);
    /* } */

  /* free the layout object */
  g_object_unref (layout);
}

void draw_graph(cairo_t * cr, Graph * g) {
  #define NODE_RADIUS 15
  cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
  for (int i = 0; i < g->len_nodes; i++) {
    Node * n = g->nodes + i;
    /* draw_text(cr, n->label, n->x, n->y); */
    cairo_arc(cr, n->x, n->y, NODE_RADIUS, 0, 2 * M_PI);
    cairo_fill(cr);
  }
  for (int i = 0; i < g->len_edges; i++) {
    Edge * e = g->edges + i;
    Node * start = graph_get_node(g, e->start_label);
    Node * end = graph_get_node(g, e->end_label);
    if (!start) {
      fprintf(stderr, "Could not find start node %s\n", e->start_label);
      continue;
    }
    if (!end) {
      fprintf(stderr, "Could not find end node %s\n", e->end_label);
      continue;
    }
    cairo_move_to(cr, start->x, start->y);
    cairo_line_to(cr, end->x, end->y);
    cairo_stroke(cr);
  }
}

static void do_draw(cairo_surface_t *sfc)
{
  cairo_t * cr = cairo_create (sfc);
  cairo_set_source_rgb (cr, 0.0, 0.0, 0.0);
  cairo_paint (cr);
  /* draw_text (cr, "Hello World", 249, 249); */
  Graph * g = graph_alloc(3, 3);
  node_init(g->nodes + 0, "A", 50, 50);
  node_init(g->nodes + 1, "B", 450, 50);
  node_init(g->nodes + 2, "C", 250, 450);
  edge_init(g->edges + 0, "A", "B");
  edge_init(g->edges + 1, "B", "C");
  edge_init(g->edges + 2, "C", "A");
  draw_graph(cr, g);
  graph_dealloc(g);
  cairo_destroy (cr);
  cairo_surface_flush(sfc);
  XFlush(cairo_xlib_surface_get_display(sfc));
}

/*! Check for Xlib Mouse/Keypress events. All other events are discarded.
 * @param sfc Pointer to Xlib surface.
 * @param block If block is set to 0, this function always returns immediately
 * and does not block. if set to a non-zero value, the function will block
 * until the next event is received.
 * @return The function returns 0 if no event occured (and block is set). A
 * positive value indicates that a key was pressed and the X11 key symbol as
 * defined in <X11/keysymdef.h> is returned. A negative value indicates a mouse
 * button event. -1 is button 1 (left button), -2 is the middle button, and -3
 * the right button.
 */
int cairo_check_event(cairo_surface_t *sfc, int block)
{
   char keybuf[8];
   KeySym key;
   XEvent e;

   for (;;)
   {
      if (block || XPending(cairo_xlib_surface_get_display(sfc)))
         XNextEvent(cairo_xlib_surface_get_display(sfc), &e);
      else
         return 0;

      switch (e.type)
      {
         case ButtonPress:
            return -e.xbutton.button;
         case KeyPress:
            XLookupString(&e.xkey, keybuf, sizeof(keybuf), &key, NULL);
            return key;
         case Expose:
            do_draw(sfc);
            break;
         default:
            fprintf(stderr, "Dropping unhandled XEevent.type = %d.\n", e.type);
      }
   }
}


/*! Open an X11 window and create a cairo surface base on that window.
 * @param x Width of window.
 * @param y Height of window.
 * @return Returns a pointer to a valid Xlib cairo surface. The function does
 * not return on error (exit(3)).
 */
cairo_surface_t *cairo_create_x11_surface0(int x, int y)
{
   Display *dsp;
   Drawable da;
   int screen;
   cairo_surface_t *sfc;

   if ((dsp = XOpenDisplay(NULL)) == NULL)
      exit(1);
   screen = DefaultScreen(dsp);
   da = XCreateSimpleWindow(dsp, DefaultRootWindow(dsp), 0, 0, x, y, 0, 0, 0);
   XSelectInput(dsp, da, ButtonPressMask | KeyPressMask | ExposureMask);
   XMapWindow(dsp, da);

   sfc = cairo_xlib_surface_create(dsp, da, DefaultVisual(dsp, screen), x, y);
   cairo_xlib_surface_set_size(sfc, x, y);

   return sfc;
}


/*! Destroy cairo Xlib surface and close X connection.
 */
void cairo_close_x11_surface(cairo_surface_t *sfc)
{
   Display *dsp = cairo_xlib_surface_get_display(sfc);

   cairo_surface_destroy(sfc);
   XCloseDisplay(dsp);
}


int main(int argc, char **argv)
{
   cairo_surface_t *sfc;

   sfc = cairo_create_x11_surface0(500, 500);

   cairo_check_event(sfc, 1);

   cairo_close_x11_surface(sfc);

   return 0;
}
