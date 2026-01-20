open Geometry

(*TODO : changer le nom du module*)
module V : Vector = VectorImpl

(* On a un problème quand on compare des floats c'est que 3. peut être égal à  3.00000002 ou 3.00000001 donc 
quand on fait un égal dessus ça peut poser problème*)

(*marge d'erreur*)
let eps = 1e-6
(*float -> float -> bool*)
let approx a b =
  Float.abs (a -. b) < eps

let approx_vec ((x1,y1) : V.t) ((x2,y2) : V.t) =
  approx x1 x2 && approx y1 y2


(* Tests (++ , --)*)

(* addition : cas simple *)
let%test _ =
  approx_vec ((1.,2.) V.++ (3.,4.)) (4.,6.)

(* addition : avec négatifs *)
let%test _ =
  approx_vec ((-1.,2.) V.++ (3.,-4.)) (2.,-2.)

(* soustraction : cas simple *)
let%test _ =
  approx_vec ((5.,7.) V.-- (2.,3.)) (3.,4.)

(* soustraction : avec négatifs *)
let%test _ =
  approx_vec ((-1.,2.) V.-- (3.,-4.)) (-4.,6.)


(*  Tests produit scalaire*)

let%test _ =
  approx ((1.,0.) V.**. (0.,1.)) 0.

let%test _ =
  approx ((2.,3.) V.**. (4.,5.)) 23.

(* avec négatifs *)
let%test _ =
  approx ((-2.,3.) V.**. (4.,-5.)) (-23.)


(* Tests scalaire * vecteur (***) *)

let%test _ =
  approx_vec (2. V.*** (3.,4.)) (6.,8.)

(* scalaire négatif *)
let%test _ =
  approx_vec ((-2.) V.** (3.,4.)) (-6.,-8.)

(* scalaire zéro *)
let%test _ =
  approx_vec (0. V.** (3.,4.)) (0.,0.)


(* Tests division d'un vecteur par un float (//)*)

let%test _ =
  approx_vec ((6.,8.) (V.//) 2.) (3.,4.)

(* division avec négatifs *)
let%test _ =
  approx_vec ((-6.,8.) V.// 2.) (-3.,4.)

(*TODO*)
(*division par 0 : à voir comment tu veux le gérer *)


(* Tests length *)

let%test _ =
  approx (V.length (3.,4.)) 5.

(* vecteur nul *)
let%test _ =
  approx (V.length (0.,0.)) 0.

(* longueur avec négatifs : length(-3, -4) = 5 *)
let%test _ =
  approx (V.length (-3.,-4.)) 5.


(* Tests normalize*)

let%test _ =
  approx_vec (V.normalize (2.,0.)) (1.,0.)

let%test _ =
  approx_vec (V.normalize (0.,2.)) (0.,1.)

let%test _ =
  approx_vec (V.normalize (0.,0.)) (0.,0.)

let%test _ =
  approx_vec (V.normalize (3.,4.)) (0.6, 0.8)


(* Tests rotate*)

let%test _ =
  approx_vec (V.rotate (3.,4.) 0.) (3.,4.)

let%test _ =
  approx_vec (V.rotate (1.,0.) (Float.pi /. 2.)) (0.,1.)

let%test _ =
  approx_vec (V.rotate (1.,0.) Float.pi) (-1.,0.)

let%test _ =
  approx_vec (V.rotate (1.,0.) (2. *. Float.pi)) (1.,0.)


(* Tests angle *)

let%test _ =
  approx (V.angle (1.,0.)) 0.

let%test _ =
  approx (V.angle (0.,1.)) (Float.pi /. 2.)

let%test _ =
  approx (V.angle (-1.,0)) Float.pi

let%test _ =
  approx (V.angle (0.,-1.)) (-.Float.pi /. 2.)

(* vecteur nul *)
let%test _ =
  approx (V.angle (0.,0.)) 0.


(* Tests project *)

(* projection sur x *)
let%test _ =
  approx_vec (V.project (3.,4.) (1.,0.)) (3.,0.)

(* projection sur y *)
let%test _ =
  approx_vec (V.project (3.,4.) (0.,1.)) (0.,4.)

let%test _ =
  approx_vec (V.project (3.,4.) (2.,0.)) (3.,0.)

let%test _ =
  approx_vec (V.project (3.,4.) (0.,2.)) (0.,4.)

(* projection sur axe nul *)
let%test _ =
  approx_vec (V.project (3.,4.) (0.,0.)) (0.,0.)

(* projection d'un vecteur déjà sur l'axe x :*)
let%test _ =
  approx_vec (V.project (5.,0.) (1.,0.)) (5.,0.)


(*Tests symetric *)

(* par rapport à l'axe x *)
let%test _ =
  approx_vec (V.symetric (3.,4.) (1.,0.)) (3.,-4.)

(* par rapport à l'axe y *)
let%test _ =
  approx_vec (V.symetric (3.,4.) (0.,1.)) (-3.,4.)

let%test _ =
  approx_vec (V.symetric (3.,4.) (2.,0.)) (3.,-4.)

let%test _ =
  approx_vec (V.symetric (3.,4.) (0.,2.)) (-3.,4.)

(* axe nul *)
let%test _ =
  approx_vec (V.symetric (3.,4.) (0.,0.)) (3.,4.)

let%test _ =
  approx_vec (V.symetric (5.,0.) (1.,0.)) (5.,0.)


(* Tests ortho*)

let%test _ =
  approx_vec (V.ortho (3.,4.)) (-4.,3.)

(* cas simple *)
let%test _ =
  approx_vec (V.ortho (1.,0.)) (0.,1.)

(* vecteur nul *)
let%test _ =
  approx_vec (V.ortho (0.,0.)) (0.,0.)
