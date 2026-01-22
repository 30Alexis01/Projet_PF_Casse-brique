open Geometry

(* marge d'erreur *)
let eps = 1e-6

(* float -> float -> bool *)
let approx a b =
  Float.abs (a -. b) < eps

let approx_vec ((x1,y1) : VectorImpl.t) ((x2,y2) : VectorImpl.t) =
  approx x1 x2 && approx y1 y2


(* Tests (++ , --) *)

(* addition : cas simple *)
let%test _ =
  approx_vec (VectorImpl.(++) (1.,2.) (3.,4.)) (4.,6.)

(* addition : avec négatifs *)
let%test _ =
  approx_vec (VectorImpl.(++) (-1.,2.) (3.,-4.)) (2.,-2.)

(* soustraction : cas simple *)
let%test _ =
  approx_vec (VectorImpl.(--) (5.,7.) (2.,3.)) (3.,4.)

(* soustraction : avec négatifs *)
let%test _ =
  approx_vec (VectorImpl.(--) (-1.,2.) (3.,-4.)) (-4.,6.)


(* Tests produit scalaire ( **. ) *)

let%test _ =
  approx (VectorImpl.( **.) (1.,0.) (0.,1.)) 0.

let%test _ =
  approx (VectorImpl.( **.) (2.,3.) (4.,5.)) 23.

(* avec négatifs *)
let%test _ =
  approx (VectorImpl.( **.) (-2.,3.) (4.,-5.)) (-23.)


(* Tests scalaire * vecteur ( *** ) *)

let%test _ =
  approx_vec (VectorImpl.( ***) 2. (3.,4.)) (6.,8.)

(* scalaire négatif *)
let%test _ =
  approx_vec (VectorImpl.( ***) (-2.) (3.,4.)) (-6.,-8.)

(* scalaire zéro *)
let%test _ =
  approx_vec (VectorImpl.( ***) 0. (3.,4.)) (0.,0.)


(* Tests division d'un vecteur par un float (//) *)

let%test _ =
  approx_vec (VectorImpl.(//) (6.,8.) 2.0) (3.,4.)

(* division avec négatifs *)
let%test _ =
  approx_vec (VectorImpl.(//) (-6.,8.) 2.) (-3.,4.)

(* division par 0 : à définir *) (*Lance une exception*)


(* Tests length *)

let%test _ =
  approx (VectorImpl.length (3.,4.)) 5.

(* vecteur nul *)
let%test _ =
  approx (VectorImpl.length (0.,0.)) 0.

(* longueur avec négatifs *)
let%test _ =
  approx (VectorImpl.length (-3.,-4.)) 5.


(* Tests normalize *)

let%test _ =
  approx_vec (VectorImpl.normalize (2.,0.)) (1.,0.)

let%test _ =
  approx_vec (VectorImpl.normalize (0.,2.)) (0.,1.)

let%test _ =
  approx_vec (VectorImpl.normalize (0.,0.)) (0.,0.)

let%test _ =
  approx_vec (VectorImpl.normalize (3.,4.)) (0.6, 0.8)


(* Tests rotate *)

let%test _ =
  approx_vec (VectorImpl.rotate (3.,4.) 0.) (3.,4.)

let%test _ =
  approx_vec (VectorImpl.rotate (1.,0.) (Float.pi /. 2.)) (0.,1.)

let%test _ =
  approx_vec (VectorImpl.rotate (1.,0.) Float.pi) (-1.,0.)

let%test _ =
  approx_vec (VectorImpl.rotate (1.,0.) (2. *. Float.pi)) (1.,0.)


(* Tests angle *)

let%test _ =
  approx (VectorImpl.angle (1.,0.)) 0.

let%test _ =
  approx (VectorImpl.angle (0.,1.)) (Float.pi /. 2.)

let%test _ =
  approx (VectorImpl.angle (-1.,0.)) Float.pi

let%test _ =
  approx (VectorImpl.angle (0.,-1.)) (-.Float.pi /. 2.)

(* vecteur nul *)
let%test _ =
  approx (VectorImpl.angle (0.,0.)) 0.


(* Tests project *)

(* projection sur x *)
let%test _ =
  approx_vec (VectorImpl.project (3.,4.) (1.,0.)) (3.,0.)

(* projection sur y *)
let%test _ =
  approx_vec (VectorImpl.project (3.,4.) (0.,1.)) (0.,4.)

let%test _ =
  approx_vec (VectorImpl.project (3.,4.) (2.,0.)) (3.,0.)

let%test _ =
  approx_vec (VectorImpl.project (3.,4.) (0.,2.)) (0.,4.)

(* projection sur axe nul *)
let%test _ =
  approx_vec (VectorImpl.project (3.,4.) (0.,0.)) (0.,0.)

(* projection d'un vecteur déjà sur l'axe x *)
let%test _ =
  approx_vec (VectorImpl.project (5.,0.) (1.,0.)) (5.,0.)


(* Tests symetric *)

(* par rapport à l'axe x *)
let%test _ =
  approx_vec (VectorImpl.symetric (3.,4.) (1.,0.)) (3.,-4.)

(* par rapport à l'axe y *)
let%test _ =
  approx_vec (VectorImpl.symetric (3.,4.) (0.,1.)) (-3.,4.)

let%test _ =
  approx_vec (VectorImpl.symetric (3.,4.) (2.,0.)) (3.,-4.)

let%test _ =
  approx_vec (VectorImpl.symetric (3.,4.) (0.,2.)) (-3.,4.)

  let%test _ =
  approx_vec (VectorImpl.symetric (-3.,-4.) (0.,2.)) (3.,-4.)

(* axe nul *) (*lève une exception*)
(*let%test _ =
  approx_vec (VectorImpl.symetric (3.,4.) (0.,0.)) (3.,4.)*)

let%test _ =
  approx_vec (VectorImpl.symetric (5.,0.) (1.,0.)) (5.,0.)


(* Tests ortho *)

let%test _ =
  approx_vec (VectorImpl.ortho (3.,4.)) (-4.,3.)

let%test _ =
  approx_vec (VectorImpl.ortho (1.,0.)) (0.,1.)

(* vecteur nul *)
let%test _ =
  approx_vec (VectorImpl.ortho (0.,0.)) (0.,0.)
