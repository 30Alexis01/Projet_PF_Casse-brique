open Geometry
module type Physic =
sig
  module V : Vector with type t = float * float

  (* élément physique de la scene :
   * forme, position, vitesse, accélération, peu rebondir *)
  type _ body = (Shape.t * V.t * V.t * V.t * bool) (* public *)
  (* ensemble de paire (élément physique, obj 'a) *)
  type (_, 'a) pool
  (* type fantome destiné au typage de body *)
  type dyna = unit (* élément mobile *)
  type stat = unit (* élément statique *)
  
  (* Pool dynamique vide *)
  val empty_dyna_pool : (dyna, _) pool
  (* Pool statique vide *)
  val empty_stat_pool : (stat, _) pool
  (* Creation body dynamique *)
  val dyna_body : Shape.t -> V.t -> V.t -> V.t -> bool -> dyna body
  (* Creation body statique *)
  val stat_body : Shape.t -> V.t -> stat body
  
  (* Ajoute un body a une pool *)
  (* return : (nouv pool * indice du body créer) *)
  val add : ('t, 'a) pool -> 't body -> 'a -> ('t, 'a) pool
  
  (* N-eme élément d'une pool *)
  val get : ('t, 'a) pool -> int -> ('t body * 'a)
  
  (* Modifie le n-eme élément d'une pool *)
  val set : ('t, 'a) pool -> int -> ('t body * 'a) -> ('t, 'a) pool
  
  (* Retire le n-eme élément d'une pool *)
  val pop : ('t, 'a) pool -> int -> ('t, 'a) pool
  
  (* Avance d'un pas de temps avec gestion des rebonds
   * return : (nouvelle pool dynamique *
   *   liste des collisions aillant eu lieu *
   *   liste des objets ayant subi une collision) *)
  val update : (dyna, 'a) pool -> (stat, 'a) pool -> float
    -> ((dyna, 'a) pool * int list * 'a list * int list * 'a list)
  
  (* Appel une fonction sur chaque body d'une pool *)
  val iter : ('t, 'a) pool -> ('t body -> 'a -> unit) -> unit
end

module ArrayBasedPhysic (V : Vector) : Physic =
struct
  module V = V

  type _ body = (Shape.t * V.t * V.t * V.t * bool)
  type ('t, 'a) pool = ('t body * 'a) list
  type stat = unit
  type dyna = unit
  
  let empty_dyna_pool = []
  let empty_stat_pool = []
  let dyna_body shape pos vit acc can_bounce = (shape, pos, vit, acc, can_bounce)
  let stat_body shape pos = (shape, pos, (0., 0.), (0., 0.), false)
  
  let add p b e = (b, e)::p
  
  let rec get p i = if i < 0 then failwith "negative index"
    else match p with
      | t::_ when i = 0 -> t
      | _::q -> get q (i-1)
      | [] -> failwith "index out of bound"
  
  let rec set p i b = if i < 0 then failwith "negative index"
    else match p with
      | _::q when i = 0 -> b::q
      | t::q -> t::(set q (i-1) b)
      | [] -> failwith "index out of bound"
  
  let rec pop p i = if i < 0 then failwith "negative index"
    else match p with
      | _::q when i = 0 -> q
      | t::q -> t::(pop q (i-1))
      | [] -> failwith "index out of bound"
  
  let update dp _ _ = (dp, [], [], [], [])
  
  let iter p f = List.iter (fun (b, e) -> f b e) p
end