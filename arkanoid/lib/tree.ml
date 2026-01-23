module type Tree =
sig
  type ('a, 'b) t =
    | Empty
    (* noeud : nombre des feuille dans le sa (sous arbre), 'b, sa gauche, sa droit*)
    | Node of (int * 'b * ('a, 'b) t * ('a, 'b) t)
    | Leaf of 'a

  (* Fonctions d'appelles des constructeurs *)
  val empty : ('a, 'b) t
  val node : 'b -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val leaf : 'a -> ('a, 'b) t

  (* Insere une feuille a une certaine position :
   * remplace la feuille deja presente par un noeud contenant l'ancienne feuille et la nouvelle *)
  val add : ('a, 'b) t -> int -> ('a * 'b) -> 'b -> ('a, 'b) t

  (* N-eme feuille en parcours infixe *)
  val get : ('a, 'b) t -> int -> 'a

  (* Modifie la n-eme feuille en parcours infixe *)
  val set : ('a, 'b) t -> int -> 'a -> ('a, 'b) t

  (* Retire la n-eme feuille en parcours infixe, supprime aussi le noeud parent et le remplace par le sa restant *)
  val pop : ('a, 'b) t -> int -> ('a, 'b) t

  val map : ('a, 'b) t -> ('a -> 'a) -> ('a, 'b) t
  val map_i : ('a, 'b) t -> ('a -> int -> 'a) -> ('a, 'b) t

  (* Parcours infixe des feuilles et appelle f sur chacune *)
  val iter : ('a, 'b) t -> ('a -> unit) -> unit

  (* Parcours infixe des feuilles à condition que les noeud parent vérifient tous une condition et appelle f sur chacune *)
  val iter_cond : ('a, 'b) t -> ('a -> unit) -> ('b -> bool) -> unit
  val iter_cond_i : ('a, 'b) t -> ('a -> int -> unit) -> ('b -> bool) -> unit
end

module Tree : Tree =
struct
  type ('a, 'b) t =
    | Empty
    (* noeud : nombre des feuille dans le sa (sous arbre), 'b, sa gauche, sa droit*)
    | Node of (int * 'b * ('a, 'b) t * ('a, 'b) t)
    | Leaf of 'a

  let rec nb_leaves t  =
    match t with
    | Empty -> 0
    | Leaf _ -> 1
    | Node (n, _, _, _) -> n

  (* Fonctions d'appelles des constructeurs *)
  let empty = Empty

  let leaf x  = Leaf x

  let node (b : 'b) (g : ('a,'b) t) (d : ('a,'b) t)=
    let n = nb_leaves g + nb_leaves d in
    Node (n, b, g, d)

    (*Vérifit que l'indice est ok*)
  let check_index t i   =
    if i < 0 || i >= nb_leaves t then
      invalid_arg "index hors limites"

  (* Insere une feuille a une certaine position :
   * remplace la feuille deja presente par un noeud contenant l'ancienne feuille et la nouvelle *)
  let rec add (t : ('a,'b) t) (i : int) ((x, _o) : ('a * 'b)) (b_parent : 'b)=
    match t with
    |Empty ->
        if i = 0 then Leaf x else invalid_arg "add: index hors limites"
    |Leaf old ->
        if i <> 0 then invalid_arg "add: index hors limites";
        node b_parent (Leaf old) (Leaf x)
    |Node (_n, b, g, d) ->
        let ng = nb_leaves g in
        if i < ng then
          node b (add g i (x, _o) b_parent) d
        else
          node b g (add d (i - ng) (x, _o) b_parent)

  (* N-eme feuille en parcours infixe *)
  let rec get t  i   =
    check_index t i;
    match t with
    | Empty -> failwith "impossible"
    | Leaf x -> x
    | Node (_n, _b, g, d) ->
        let ng = nb_leaves g in
        if i < ng then get g i else get d (i - ng)

  (* Modifie la n-eme feuille en parcours infixe *)
  let rec set t  i  x   =
    check_index t i;
    match t with
    | Empty -> failwith "impossible"
    | Leaf _ -> Leaf x
    | Node (_n, b, g, d) ->
        let ng = nb_leaves g in
        if i < ng then
          node b (set g i x) d
        else
          node b g (set d (i - ng) x)

  (* Retire la n-eme feuille en parcours infixe, supprime aussi le noeud parent et le remplace par le sa restant *)
  let rec pop t i  =
    check_index t i;
    match t with
    | Empty -> failwith "impossible de modifier un arbre vide"
    | Leaf _ -> Empty
    | Node (_n, b, g, d) ->
        let ng = nb_leaves g in
        if i < ng then
          let g' = pop g i in
          (match g' with
           | Empty -> d
           | _ -> node b g' d)
        else
          let d' = pop d (i - ng) in
          (match d' with
           | Empty -> g
           | _ -> node b g d')

  (* Parcours infixe des feuilles et appelle f sur chacune *)
  let rec iter t f  =
    match t with
    | Empty -> ()
    | Leaf x -> f x
    | Node (_n, _b, g, d) ->
        iter g f;
        iter d f

  (* Parcours infixe des feuilles à condition que les noeud parent vérifient tous une condition et appelle f sur chacune *)
  let iter_cond t f cond  =
    let rec aux (ok : bool) (t : ('a,'b) t) : unit =
      match t with
      | Empty -> ()
      | Leaf x -> if ok then f x
      | Node (_n, b, g, d) ->
          let ok' = ok && cond b in
          if ok' then (aux ok' g; aux ok' d) else ()
    in
    aux true t

  (*comme iter_cond mais donne l'indice de la feuille en parametre*)
  let iter_cond_i t f cond  =
    let rec aux (ok : bool) (offset : int) (t : ('a,'b) t) : unit =
      match t with
      | Empty -> ()
      | Leaf x -> if ok then f x offset
      | Node (_n, b, g, d) ->
          let ok' = ok && cond b in
          if not ok' then ()
          else
            let ng = nb_leaves g in
            aux ok' offset g;
            aux ok' (offset + ng) d
    in
    aux true 0 t
end