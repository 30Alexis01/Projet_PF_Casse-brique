open Color
open Geometry
open Iterator
open Physic

(* Module de représentation de jeux, on y décrit l'ensemble de la logique *)
module type GameFunctorResult =
sig
  type vector
  type scene
  type entity

  (* Création d'une nouvelle scene *)
  val start : unit -> scene Flux.t

  (* Itere sur tous les objets graphique de la scene et appelle une fonction d'affichage *)
  val draw : scene -> (Shape.t -> vector -> Color.t -> unit) -> unit
end

(* Foncteur permettant de généré un Game *)
module type Game = functor (V : Vector) (PF : Physic) -> GameFunctorResult with type vector = V.t