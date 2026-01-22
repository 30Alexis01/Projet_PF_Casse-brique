open Lib_arkanoid

open Color
open Game
open Geometry
open Iterator
open Physic

module type Arkanoid = functor (V : Vector) (PF : Physic) ->
sig
  include GameFunctorResult with type vector = V.t
  val score : scene -> int
end

module ArkanoidGame : Arkanoid = functor (V : Vector) (PF : Physic) ->
struct
  type vector = V.t (* nécéssaire pour respecter la signature mais pas utile en pratique *)
  module P = PF (V)

  type entity = Ball | Racket | Brick of int | MapBorder of bool
  type scene = (int * int * (P.dyna, entity) P.pool * (P.stat, entity) P.pool)

  let init_scene = let stat_pool = List.fold_left
    (fun pool (body, obj) -> P.add pool body obj)
    P.empty_stat_pool
    [
      ((Shape.Rect (10., 5.), (10., 10.), (0., 0.), (0., 0.), false), Brick 2),
      ((Shape.Rect (10., 5.), (30., 10.), (0., 0.), (0., 0.), false), Brick 2),
      ((Shape.Rect (10., 5.), (10., 18.), (0., 0.), (0., 0.), false), Brick 1),
      ((Shape.Rect (10., 5.), (30., 18.), (0., 0.), (0., 0.), false), Brick 1),
      ((Shape.Rect (90., 90.), (45., -45.), (0., 0.), (0., 0.), false),MapBorder false),
      ((Shape.Rect (90., 90.), (-45., 45.), (0., 0.), (0., 0.), false),MapBorder false),
      ((Shape.Rect (90., 90.), (135., 45.), (0., 0.), (0., 0.), false),MapBorder false),
      ((Shape.Rect (90., 90.), (45., 135.), (0., 0.), (0., 0.), false),MapBorder true),
    ]
  in let dyna_pool = List.fold_left
    (fun pool (body, obj) -> P.add pool body obj)
    P.empty_dyna_pool
    [
      ((Shape.Rect (20., 2.), (45., 80.), (0., 0.), (0., 0.), false), Racket),
      ((Shape.Circle 3., (45., 70.), (0., -10.), (0., 8.), true), Ball),
    ]
  in (5, 0, dyna_pool, stat_pool)


  let update (pv, score, pool1, pool2) = (pv, score, pool1, pool2)

  let start () = Flux.unfold (fun s -> let s' = update s in Some ((s', s'))) init

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