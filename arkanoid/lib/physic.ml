open Geometry
open Tree

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

  (* Premier élément verifiant cond *)
  val get_cond : ('t, 'a) pool -> ('a -> bool) -> (int * 't body * 'a) option
  
  (* Modifie le n-eme élément d'une pool
   * postcondition : si la pool est statique alors le body n'a pas changé*)
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

module TreeBasedPhysic (V : Vector) (T : Tree) : Physic =
struct
  module V = V

  type aabb = (V.t * V.t)
  type _ body = (Shape.t * V.t * V.t * V.t * bool)
  type ('t, 'a) pool = ('t body * 'a, aabb) T.t
  type stat = unit
  type dyna = unit

  let empty_dyna_pool = T.empty
  let empty_stat_pool = T.empty
  let dyna_body shape pos vit acc can_bounce = (shape, pos, vit, acc, can_bounce)
  let stat_body shape pos = (shape, pos, (0., 0.), (0., 0.), false)
  
  let get_aabb (shape, pos, _, _, _) = match shape with
    | Shape.Circle radius -> V.(pos -- radius *** (1., 1.), pos ++ radius *** (1., 1.))
    | Shape.Rect (w, h) -> V.(pos -- 0.5 *** (w, h), pos ++ 0.5 *** (w, h))
    | _ -> ((0.,0.), (0.,0.))

  let union (v11, v21) (v12, v22) =
    match ((v11, v21), (v12, v22)) with
    | (((0.,0.), (0.,0.)), (v1, v2)) -> (v1, v2)
    | ((v1, v2), ((0.,0.), (0.,0.))) -> (v1, v2)
    | (((x11, y11), (x21, y21)), ((x12, y12), (x22, y22))) -> 
      ((min x11 x12, min y11 y12), (max x21 x22, max y21 y22))

  let volume (v1, v2) = let (w, h) = V.(v1 -- v2) in (abs_float w) *. (abs_float h)

  let overlap ((x11, y11), (x12, y12)) ((x21, y21), (x22, y22)) =
    x12 > x21 && x22 > x11 && y12 > y21 && y22 > y11

  let union_st bb st = match st with
    | T.Empty -> bb
    | T.Leaf (b', _) -> union bb (get_aabb b')
    | T.Node (_, bb', _, _) -> union bb bb'

  let get_aabb_st st = match st with
    | T.Empty -> ((0.,0.), (0.,0.))
    | T.Leaf (b, _) -> get_aabb b
    | T.Node (_, bb, _, _) -> bb

  let add p b e = let rec aux p leaf bb =
    match p with
      | T.Empty -> leaf
      | T.Leaf (b', _) -> T.node (union bb (get_aabb b')) p leaf
      | T.Node (_, bb', st1, st2) ->
        if volume (union_st bb st1) < volume (union_st bb st2) then
          T.node (union bb bb') (aux st1 leaf bb) st2
        else
          T.node (union bb bb') st1 (aux st2 leaf bb)
  in aux p (T.leaf (b, e)) (get_aabb b)
  
  let get = T.get

  let get_cond p cond = let rec aux st i =
    match st with
      | T.Empty -> None
      | T.Leaf (b, e) -> if cond e then Some (i, b, e) else None
      | T.Node (_, _, st1, st2) -> 
        (match aux st1 i with
          | Some x -> Some x
          | None -> aux st2 (i + match st1 with T.Node (k, _, _, _) -> k | _ -> 1))
  in aux p 0
    
  let set = T.set
  
  let pop = T.pop
  
  let collide (s1, p1, _, _, _) (s2, p2, _, _, _) =
    let open V in
    match s1, s2 with
      | Shape.Circle r1, Shape.Circle r2 ->
        let delta = p2 -- p1 in
        let dist = length delta in

        if dist = 0. then
          (1., 0.)
        else if dist >= r1 +. r2 then
          (0., 0.)
        else
          normalize delta

      | Shape.Rect (w1, h1), Shape.Rect (w2, h2) ->
        let (dx, dy) = p2 -- p1 in
        let overlap_x = 0.5 *. (w1 +. w2) -. abs_float dx in
        let overlap_y = 0.5 *. (h1 +. h2) -. abs_float dy in

        if overlap_x <= 0. || overlap_y <= 0. then
          (0., 0.)
        else if overlap_x < overlap_y then
          if dx > 0. then (1., 0.) else (-1., 0.)
        else
          if dy > 0. then (0., 1.) else (0., -1.)

      | Shape.Circle r, Shape.Rect (w, h)
      | Shape.Rect (w, h), Shape.Circle r ->
        let (cx, cy) = match s1 with Shape.Circle _ -> p1 | _ -> p2 in
        let (rx, ry) = match s1 with Shape.Rect _ -> p1 | _ -> p2 in

        let closest_x = max (rx -. w /. 2.) (min cx (rx +. w /. 2.)) in
        let closest_y = max (ry -. h /. 2.) (min cy (ry +. h /. 2.)) in
        let delta = (cx -. closest_x, cy -. closest_y) in
        let dist = length delta in

        if dist = 0. then
          (1., 0.)
        else if dist >= r then
          (0., 0.)
        else
          normalize delta

      | _ -> (0., 0.)

  let update_body dp sp dt ((s, pos, vit, acc, bounce), e) i =
    let rec get_collision_list p b bb i j vit =
      match p with
        | T.Empty -> ([], vit)
        | T.Leaf (b', e) -> 
          if i = j then ([], vit) else
          let n = collide b b' in
            if V.(n **. vit) < 0. then
              ([(j, e)], if bounce then V.(symetric vit (ortho n)) else (0., 0.))
            else
              ([], vit)
        | T.Node (_, bb', st1, st2) ->
          if overlap bb bb' then
            let (l1, vit1) = get_collision_list st1 b bb i j vit in
            let (l2, vit2) = (get_collision_list st2 b bb i
              (j + match st1 with T.Node (k, _, _, _) -> k | _ -> 1) vit1) in
            (l1 @ l2, vit2)
          else
            ([], vit)
    in
      let b = V.((s, pos ++ dt *** vit, vit ++ dt *** acc, acc, bounce)) in
      let bb = get_aabb b in
      let (l2, vit2) = get_collision_list sp b bb (-1) 0 vit in
      let (l1, vit1) = get_collision_list dp b bb i 0 vit2 in

        match (l1, l2) with
          | [], [] -> (b, [], [])
          | _, stat_col -> ((s, pos, vit1, acc, bounce), [(i, e)], stat_col)

  let update dp sp dt =
    let rec aux st i =
      match st with
        | T.Empty -> (T.empty, [], [])
        | T.Leaf (b,e) -> let (b', cd, cs) = update_body dp sp dt (b,e) i in (T.leaf (b',e), cd, cs)
        | T.Node (_, _, st1, st2) ->
          let (nst1, cd1, cs1) = aux st1 i in
          let (nst2, cd2, cs2) = aux st2 (i + match st1 with T.Node (k, _, _, _) -> k | _ -> 1) in
          (T.node (union (get_aabb_st nst1) (get_aabb_st nst2)) nst1 nst2, cd1 @ cd2, cs1 @ cs2)
    in
    let sep = List.fold_left (fun (l1, l2) (e1, e2) -> (e1::l1, e2::l2)) ([], []) in
    let (ndp, cd, cs) = aux dp 0 in
    let (cdi, cde) = sep cd in
    let (csi, cse) = sep cs in
    (ndp, cdi, cde, csi, cse)
  
  let iter p f = T.iter p (fun (b, e) -> f b e)
end