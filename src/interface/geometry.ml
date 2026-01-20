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
	val ( *. ) : t -> t -> float
	
	(* Produit d'un float par un vecteur *)
	val ( ** ) : float -> t -> t
	
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