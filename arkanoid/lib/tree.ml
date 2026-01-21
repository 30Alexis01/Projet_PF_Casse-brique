module type Tree = struct
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

  (* Parcours infixe des feuilles et appelle f sur chacune *)
  val iter : ('a, 'b) t -> ('a -> unit) -> unit

  (* Parcours infixe des feuilles à condition que les noeud parent vérifient tous une condition et appelle f sur chacune *)
  val iter_cond : ('a, 'b) t -> ('a -> unit) -> ('b -> bool) -> unit
end