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

(* Petit helper "élève" pour que les assert soient lisibles *)
let assert_float valeur_calculee valeur_attendue =
  assert (approx valeur_calculee valeur_attendue)

let assert_vec valeur_calculee valeur_attendue =
  assert (approx_vec valeur_calculee valeur_attendue)

let () =

  (* Tests (++ , --)*)


  (* addition : cas simple *)
  assert_vec ((1.,2.) V.++ (3.,4.)) (4.,6.);

  (* addition : avec négatifs *)
  assert_vec ((-1.,2.) V.++ (3.,-4.)) (2.,-2.);

  (* soustraction : cas simple *)
  assert_vec ((5.,7.) V.-- (2.,3.)) (3.,4.);

  (* soustraction : avec négatifs *)
  assert_vec ((-1.,2.) V.-- (3.,-4.)) (-4.,6.);


  (*  Tests produit scalaire*)

  assert_float ((1.,0.) V.*. (0.,1.)) 0.;

  assert_float ((2.,3.) V.*. (4.,5.)) 23.;

  (* avec négatifs *)
  assert_float ((-2.,3.) V.*. (4.,-5.)) (-23.);


  (* Tests scalaire * vecteur (**) *)

  assert_vec (2. V.** (3.,4.)) (6.,8.);

  (* scalaire négatif *)
  assert_vec ((-2.) V.** (3.,4.)) (-6.,-8.);

  (* scalaire zéro *)
  assert_vec (0. V.** (3.,4.)) (0.,0.);


  (* Tests division d'un vecteur par un float (//)*)
  

  assert_vec ((6.,8.) V.// 2.) (3.,4.);

  (* division avec négatifs *)
  assert_vec ((-6.,8.) V.// 2.) (-3.,4.);


  (*TODO*)
  (*division par 0 : à voir comment tu veux le gérer *)


 
  (* Tests length *)

  assert_float (V.length (3.,4.)) 5.;

  (* vecteur nul *)
  assert_float (V.length (0.,0.)) 0.;

  (* longueur avec négatifs : length(-3, -4) = 5 *)
  assert_float (V.length (-3.,-4.)) 5.;


  (* Tests normalize*)

  assert_vec (V.normalize (2.,0.)) (1.,0.);

  assert_vec (V.normalize (0.,2.)) (0.,1.);

  assert_vec (V.normalize (0.,0.)) (0.,0.);

  assert_vec (V.normalize (3.,4.)) (0.6, 0.8);


  (* Tests rotate*)


  assert_vec (V.rotate (3.,4.) 0.) (3.,4.);

  assert_vec (V.rotate (1.,0.) (Float.pi /. 2.)) (0.,1.);

  assert_vec (V.rotate (1.,0.) Float.pi) (-1.,0.);

 
  assert_vec (V.rotate (1.,0.) (2. *. Float.pi)) (1.,0.);


  (* Tests angle *)

  assert_float (V.angle (1.,0.)) 0.;
  assert_float (V.angle (0.,1.)) (Float.pi /. 2.);
  assert_float (V.angle (-1.,0)) Float.pi;

  assert_float (V.angle (0.,-1.)) (-.Float.pi /. 2.);

  (* vecteur nul *)
  assert_float (V.angle (0.,0.)) 0.;


  
  (* Tests project *)

  (* projection sur x *)
  assert_vec (V.project (3.,4.) (1.,0.)) (3.,0.);

  (* projection sur y *)
  assert_vec (V.project (3.,4.) (0.,1.)) (0.,4.);

  assert_vec (V.project (3.,4.) (2.,0.)) (3.,0.);

  assert_vec (V.project (3.,4.) (0.,2.)) (0.,4.);

  (* projection sur axe nul *)
  assert_vec (V.project (3.,4.) (0.,0.)) (0.,0.);

  (* projection d'un vecteur déjà sur l'axe x :*)
  assert_vec (V.project (5.,0.) (1.,0.)) (5.,0.);


  (*Tests symetric *)

  (* par rapport à l'axe x *)
  assert_vec (V.symetric (3.,4.) (1.,0.)) (3.,-4.);

  (* par rapport à l'axe y *)
  assert_vec (V.symetric (3.,4.) (0.,1.)) (-3.,4.);


  assert_vec (V.symetric (3.,4.) (2.,0.)) (3.,-4.);
  assert_vec (V.symetric (3.,4.) (0.,2.)) (-3.,4.);

  (* axe nul *)
  assert_vec (V.symetric (3.,4.) (0.,0.)) (3.,4.);

  assert_vec (V.symetric (5.,0.) (1.,0.)) (5.,0.);



  (* Tests ortho*)


  assert_vec (V.ortho (3.,4.)) (-4.,3.);

  (* cas simple *)
  assert_vec (V.ortho (1.,0.)) (0.,1.);

  (* vecteur nul *)
  assert_vec (V.ortho (0.,0.)) (0.,0.);


  print_endline " Vector test OK";
