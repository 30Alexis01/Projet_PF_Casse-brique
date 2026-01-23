open Lib_arkanoid

open Color
open Game
open Geometry
open Iterator
open Physic

module type Arkanoid =
sig
  include Game
  val score : scene -> int
end

module ArkanoidGame (V : Vector) (P : Physic) : Arkanoid =
struct
  module V = V
  module P = P

  type entity = Ball | Racket | Brick of int | MapBorder of bool
  (* Scene : nb balle, score, pool dyna (balle+raquete), pool stat (brick+wall) *)
  type scene = (int * int * (P.dyna, entity) P.pool * (P.stat, entity) P.pool)

  let init_scene = let stat_pool = List.fold_left
    (fun pool (body, obj) -> P.add pool body obj)
    P.empty_stat_pool
    [
      (P.stat_body (Shape.Rect (10., 5.)) (10., 10.), Brick 2);
      (P.stat_body (Shape.Rect (10., 5.)) (30., 10.), Brick 2);
      (P.stat_body (Shape.Rect (10., 5.)) (10., 18.), Brick 1);
      (P.stat_body (Shape.Rect (10., 5.)) (30., 18.), Brick 1);
      (P.stat_body (Shape.Rect (90., 90.)) (45., -45.), MapBorder false);
      (P.stat_body (Shape.Rect (90., 90.)) (-45., 45.), MapBorder false);
      (P.stat_body (Shape.Rect (90., 90.)) (135., 45.), MapBorder false);
      (P.stat_body (Shape.Rect (90., 90.)) (45., 135.), MapBorder true)
    ]
  in let dyna_pool = List.fold_left
    (fun pool (body, obj) -> P.add pool body obj)
    P.empty_dyna_pool
    [
      (P.dyna_body (Shape.Rect (20., 2.)) (45., 80.) (0., 0.) (0., 0.) false, Racket);
      (P.dyna_body (Shape.Circle 3.) (45., 70.) (0., -10.) (0., 8.) true, Ball)
    ]
  in (5, 0, dyna_pool, stat_pool)


  let update (pv, score, pool1, pool2) (mous_pos, mouss_click) dt  = (pv, score, pool1, pool2)

  let start inputs dt = Flux.unfold (fun s -> let s' = update s (F.uncons inputs) dt in Some ((s', s'))) init_scene

  let draw (_, _, pool1, pool2) f =
    let wrap = fun ((shape, pos, _, _, _) : _ P.body) obj -> f shape pos
      (match obj with
        | Ball -> Color.Cyan
        | Brick _ -> Color.Cyan
        | Racket -> Color.Black
        | MapBorder _ -> Color.White)
    in let _ = P.iter pool1 wrap
    in let _ = P.iter pool2 wrap
    in ()

    let score (_, s, _, _) = s

end