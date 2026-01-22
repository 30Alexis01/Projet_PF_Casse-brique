open Color
open Geometry
open Iterator
open Physic

(* Module de représentation de jeux, on y décrit l'ensemble de la logique *)
module type Game =
sig
  module V : Vector with type t = float * float
  module P : Physic with module V = V
  type scene
  type entity

  (* Création d'une nouvelle scene *)
  val start : unit -> scene Flux.t

  (* Itere sur tous les objets graphique de la scene et appelle une fonction d'affichage *)
  val draw : scene -> (Shape.t -> V.t -> Color.t -> unit) -> unit
end
