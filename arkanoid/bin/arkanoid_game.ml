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

  let start () = Flux.unfold (fun () -> None) ()

  let draw (_, _, pool1, pool2) f =
    let wrap = fun ((shape, pos, _, _, _) : _ P.body) obj -> f shape pos
      (match obj with
        | Ball -> Color.Blue
        | Brick _ -> Color.Blue
        | Racket -> Color.Black
        | MapBorder _ -> Color.White)
    in let _ = P.iter pool1 wrap
    in let _ = P.iter pool2 wrap
    in ()

    let score (_, s, _, _) = s

end