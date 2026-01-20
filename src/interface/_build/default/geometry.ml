module Shape =
struct
	type t = 
		(* Cercle : rayon *)
		| Circle of float
		(* Rectanble : largeur * hauteur *)
		| Rect of (float * float)
		(* Texte, sans collision : contenue * taille police *)
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
	
	(* Rotation d'un vecteur par un angle modulo 2PI *)
	val rotate : t -> float -> t
	
	(* Angle d'un vecteur avec l'axe x en sens trigo *)
	val angle : t -> float
	
	(* Projection d'un vecteur sur un axe *)
	val project : t -> t -> t
	
	(* Symetrie d'un vecteur par un axe *)
	val symetric : t -> t -> t
	
	(* Vecteur orthogonal d'un vecteur, transfo (-y, x)
	 * equivalent à rotate vec PI/2 *)
	val ortho : t -> t
end

module VectorImpl : Vector with type t = float*float =
struct
	type t = float*float
	let (++) (x1,y1) (x2,y2) = (x1+.x2, y1 +. y2)
	let (--) (x1,y1) (x2,y2) = (x1-.x2, y1 -. y2)
	let ( **. ) (x1,y1) (x2,y2) = x1*.x2 +.y1 *. y2
	let ( *** ) k (x1,y1)  = (k *. x1, k *. y1)
	let (//) (x1,y1) k = if k = 0. then failwith "Erreur : division par zero"
	else (x1/.k,y1/.k)
	(*Part du principe que length est en fait norm*)
	let length (x1,y1) = ( x1**2. +. y1**2. )**(1./.2.)
	let normalize v = let norm = length v in if norm = 0. then (0.,0.) else norm *** v 
	let rotate (x1,y1) angle = (x1*.cos(angle)+.y1*.sin(angle),y1*.cos(angle)-.x1*.sin(angle)) (* TODO : check si ça marche avec des angles pas dans la range 0-2pi*)
	let angle v = v **. (1.,0.) (* TODO check si bien en sens trigo*)
	let project v axe= ((v**.axe)***axe)// ((length axe)**(1./.2.))
	let symetric (x1,y1) (a1,a2) = let (xi,yi) = project (x1,y1) (a1,a2) in (2.*.xi -. x1, 2.*.yi +. y1)
	let ortho v = rotate v (Float.pi/.2.)

end