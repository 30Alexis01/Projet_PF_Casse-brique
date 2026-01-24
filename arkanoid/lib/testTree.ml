open Tree
module T = TreeImpl

let t0 = T.leaf "A"

(* add *)
let t1 = T.add t0 0 ("B", 0) 1

let expected_t1 =
  T.Node (2, 1, T.Leaf "A", T.Leaf "B")

let%test _ =
  t1 = expected_t1


let t2 = T.add t1 1 ("C", 0) 2

let expected_t2 =
  T.Node (3, 1,
    T.Leaf "A",
    T.Node (2, 2, T.Leaf "B", T.Leaf "C")
  )

let%test _ =
  t2 = expected_t2

(* A,B,C,D : on remplace la feuille 2 (C) par (C,D) *)
let t3 = T.add t2 2 ("D", 0) 3

let expected_t3 =
  T.Node (4, 1,
    T.Leaf "A",
    T.Node (3, 2,
      T.Leaf "B",
      T.Node (2, 3, T.Leaf "C", T.Leaf "D")
    )
  )

let%test _ =
  t3 = expected_t3


(*get*)

let%test _ = T.get t3 0 = "A"
let%test _ = T.get t3 1 = "B"
let%test _ = T.get t3 2 = "C"
let%test _ = T.get t3 3 = "D"


(*set*)

(* set feuille 1 : B -> X *)
let t4 = T.set t3 1 "X"

let expected_t4 =
  T.Node (4, 1,
    T.Leaf "A",
    T.Node (3, 2,
      T.Leaf "X",
      T.Node (2, 3, T.Leaf "C", T.Leaf "D")
    )
  )

let%test _ =
  t4 = expected_t4


(*pop*)

(* pop feuille 2 : on enlève C -> il reste A,X,D *)
let t5 = T.pop t4 2

let expected_t5 =
  T.Node (3, 1,
    T.Leaf "A",
    T.Node (2, 2,
      T.Leaf "X",
      T.Leaf "D"
    )
  )

let%test _ =
  t5 = expected_t5


(*add après pop*)

(* add à l'indice 1 : on remplace X par (X,Y) -> A,X,Y,D *)
let t6 = T.add t5 1 ("Y", 0) 9

let expected_t6 =
  T.Node (4, 1,
    T.Leaf "A",
    T.Node (3, 2,
      T.Node (2, 9, T.Leaf "X", T.Leaf "Y"),
      T.Leaf "D"
    )
  )

let%test _ =
  t6 = expected_t6


(* iter_cond / iter_cond_i *)
let t_cond =
  T.Node (4, 10,
    T.Node (2, 0, T.Leaf "A", T.Leaf "B"),
    T.Node (2, 5, T.Leaf "C", T.Leaf "D")
  )

(* cond : b > 0,  on coupe le sous-arbre gauche (b=0) et on garde le sous-arbre droit *)
let cond b = b > 0

(* Pour tester iter_cond / iter_cond_i sans helper, on accumule directement dans une ref
   (c'est pas une fonction helper, c'est juste la manière de récupérer ce qui a été visité) *)

let%test _ =
  let acc = ref [] in
  let _ = T.iter_cond t_cond (fun x -> acc := x :: !acc) cond in
  List.rev !acc = ["C"; "D"]

let%test _ =
  let acc = ref [] in
  let _ = T.iter_cond_i t_cond (fun x i -> acc := (x,i) :: !acc) cond in
  List.rev !acc = [("C",2); ("D",3)]
