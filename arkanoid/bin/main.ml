open Lib_arkanoid

open Arkanoid_game
open Geometry
open Iterator
open Physic
open Color

module Init = struct
  let dt = 1. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module AG = ArkanoidGame (VectorImpl) (TreeBasedPhysic (VectorImpl))

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))


let print_shape shape (x,y) = match shape with
  | Geometry.Shape.Circle(r) ->
    Graphics.fill_circle (int_of_float x) (int_of_float y) (int_of_float r)
  | Geometry.Shape.Rect(w,h) ->
    Graphics.fill_rect (int_of_float (x -. w /. 2.)) (int_of_float (y -. h /. 2.)) (int_of_float w) (int_of_float h)
  | Geometry.Shape.Text(t,size) -> 
    Graphics.moveto (int_of_float x) (int_of_float y);
    Graphics.set_text_size (int_of_float size);
    Graphics.draw_string t

let get_color color = match color with
  | Color.Black -> Graphics.black
  | Color.White -> Graphics.white
  | Color.Red -> Graphics.red
  | Color.Green -> Graphics.green
  | Color.Yellow -> Graphics.yellow
  | Color.Cyan -> Graphics.cyan
  | Color.Magenta -> Graphics.magenta
  

let draw_object shape pos color = (*Graphics.clear_graph ();*) (*Faut clear_graph avant d'itérer*)
              Graphics.set_color (get_color color);
              print_shape shape pos;
              Graphics.synchronize ()

(* extrait le score courant d'un etat : *)
let score = AG.score

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      AG.draw etat draw_object;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' (score etat)
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

let _ = draw (AG.start mouse Init.dt)