(* open Geometry
open Physic
open Color
open Flux *)

module type Game (V : Vector, P : Physic, I : Input) :
sig
	type scene
	type entity
	(* Création d'une nouvelle scene *)
	sig start : () -> scene Flux.t
	(* Itere sur tous les objets graphique de la scene et appelle une fonction *)
	sig draw : scene -> (Shape.t -> V.t -> Color.t -> unit) -> unit
end


module Arkanoid : Game (V : Vector2, P : Physic, E : Event) =
struct
	type entity = 
	(* balle mobile *)
	| Ball
	(* brique : pv *)
	| Brick of int
	(* raquette : vitesse max *)
	| Racket of float
	(* bordure de la map : tue la balle (pour la bordure du bas) *)
	| MapBorder of bool
	(* scene : nb de balle restante, score, pool statique, pool dynamique *)
	type scene = (int * int * pool * pool)
	
	let zero = let stat_pool = List.fold_left
		(fun pool (body, obj) -> P.add pool body obj)
		P.empty_stat_pool
		[
			((Shape.Rect (10., 5.), (10., 10.), (0., 0.), (0., 0.)), Brick 2),
			((Shape.Rect (10., 5.), (20., 10.), (0., 0.), (0., 0.)), Brick 2),
			((Shape.Rect (10., 5.), (10., 16.), (0., 0.), (0., 0.)), Brick 1),
			((Shape.Rect (10., 5.), (20., 16.), (0., 0.), (0., 0.)), Brick 1),
			((Shape.Rect (90.,90.),(0.,.-90.),(0.,0.),(0.,0.)),MapBorder false),
			
		]
	in let dyna_pool = P.add (P.add P.empty_dyna_pool (..) (Racket 10.)) (..) Ball
	in (5, 0, stat_pool, dyna_pool)
	
	(*let update input scn = *) (* c'est ici qu'on gere toute la logique propre au jeu : mvt sourie => mvt raquette; brick => -pv_brick +score; rebond => +vitesse; collision sol => -balle / game over; ... bref une grosse fonction bien moche *)
	
	let start () = Flux.unfold
		(fun scn -> let new_scn = update (uncons I.flux) scn in
			(new_scn, new_scn))
		zero
	
	let draw (_, score, pool1, pool2) f =
		let wrap = fun (shape, pos, _, _) obj -> f shape pos
			match obj with
				| Ball -> Color.Yellow
				| Brick _ -> Color.Red
				| Racket _ -> Color.Black
				| MapBorder -> Color.White
		in let _ = P.iter wrap pool1
		in let _ = P.iter wrap pool2
		in f (Text (string_from_int score) (2., 2.) 12.)
end