(* Module permettant de décrir des forme géometrique *)
module Shape =
struct
  type t = 
    (* Cercle : rayon *)
    | Circle of float
    (* Rectanble : largeur * hauteur *)
    | Rect of (float * float)
    (* Texte : contenue * taille police *)
    | Text of (string * float)
end
module type Vector =
sig
  type t = (float * float) (* public *)
  
  (* Somme canonique de 2 vecteurs *)
  val ( ++ ) : t -> t -> t
  
  (* Soustraction canonique de 2 vecteurs *)
  val ( -- ) : t -> t -> t
  
  (* Produit scalaire de 2 vecteurs *)
  val ( **. ) : t -> t -> float
  
  (* Produit d'un float par un vecteur *)
  val ( *** ) : float -> t -> t
  
  (* Division d'un vecteur par un float *)
  val ( // ) : t -> float -> t
  
  (* Longeur d'un vecteur *)
  val length : t -> float

  (* Vecteur normalise d'un vecteur *)
  val normalize : t -> t
  
  (* Rotation d'un vecteur par un angle modulo 2PI (sens trigo) *)
  val rotate : t -> float -> t
  
  (* Angle d'un vecteur avec l'axe x en sens trigo, si v=(0,0), renvoie 0 *)
  val angle : t -> float
  
  (* Projection d'un vecteur sur un axe , si l'axe est (0;0), renvoie (0,0)*)
  val project : t -> t -> t
  
  (* Symetrie d'un vecteur par un axe, si l'axe est (0,0), lève une exception *)
  val symetric : t -> t -> t
  
  (* Vecteur orthogonal d'un vecteur, transfo (-y, x)
   * equivalent à rotate vec PI/2 *)
  val ortho : t -> t
end

module VectorImpl : Vector with type t = float*float =
struct
  type t = float*float
  let ( ++ ) (x1,y1) (x2,y2) = (x1+.x2, y1 +. y2)
  let ( -- ) (x1,y1) (x2,y2) = (x1-.x2, y1 -. y2)
  let ( **. ) (x1,y1) (x2,y2) = x1*.x2 +.y1 *. y2
  let ( *** ) k (x1,y1)  = (k *. x1, k *. y1)
  let ( // ) (x1,y1) k = if k = 0. then failwith "Erreur : division par zero"
  else (x1/.k,y1/.k)
  (*Part du principe que length est en fait norm*)
  let length (x1,y1) = ( x1**2. +. y1**2. )**(1./.2.)
  let normalize v = let norm = length v in if norm = 0. then (0.,0.) else (1./.norm) *** v 
  let rotate (x1,y1) angle = (x1*.cos(angle)-.y1*.sin(angle),y1*.cos(angle)+.x1*.sin(angle))
  let angle (x,y) = if (x,y)= (0.,0.) then 0. else if y < 0. then -.acos((normalize ((x,y))) **. (1.,0.)) else acos((normalize ((x,y))) **. (1.,0.))
  let project v axe= if axe = (0.,0.) then axe else ((v**.axe)***axe)// ((length axe)**(2.))
  let symetric (x1,y1) (a1,a2) = if (a1,a2)=(0.,0.) then failwith "Pas possible de faire de symmétrie axiale par rapport à un pont"
  else let (xi,yi) = project (x1,y1) (a1,a2) in (2.*.xi -. x1, 2.*.yi -. y1)
  let ortho v = rotate v (Float.pi/.2.)

end