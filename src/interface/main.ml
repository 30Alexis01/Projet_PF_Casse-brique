open Geometry;;
open Physic;;
open Color;;
open Flux ;;
open Graphics;;

let print_function shape vector color = (*Graphics.clear_graph ();*) (*Faut clear_graph avant d'itérer*)
              Graphics.set_color (get_color color);
              print_shape shape vector;
              Graphics.synchronize ()

let print_shape shape (x,y) = match shape with
| Circle(r) -> Graphics.fill_circle x y r
| Rect(w,h) -> Graphics.fill_rect (x-w/.2.) (y-h/.2.) w h (*TODO : verifier le sens de la sortie*)
| Text(t,size) -> let len = t.length in Graphics.moveto (x-.len/.2.) (y-.len/.2.); Graphics.set_text_size size; Graphics.draw_string t

let get_color color = match color with
  | Black -> Graphics.black
  | White -> Graphics.white
  | Red -> Graphics.red
  | Green -> Graphics.green
  | Yellow -> Graphics.yellow
  | Cyan -> Graphics.cyan
  | Magenta -> Graphics.magenta