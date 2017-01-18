(*
 Les arbres binaires entiers permettent de représenter les triangulations de polygones convexes.
Les fonctions définies ici visent à implémenter l'équivalence dihédrale entre les triangulations de polygones réguliers.
L'équivalence dihédrale est définie naturellement par:
   Deux triangulations t1 et t2 d'un même n-gone régulier sont équivalentes s'il existe s dans D_n tel que t1 = s t2.
*) 



(*
Type standard pour les arbres binaires
 *)
type 'a btree = 
  |Leaf of 'a
  |Node of ('a btree)*'a*('a btree);;


type flexagone = int btree;;
type direction = |Gauche |Droite;;
type chemin = direction list;;



let (flexagone_trois:flexagone) = Node(Leaf(1),0,Leaf(2));;


  
let rec taille f = match f with
  |Leaf(_) -> 1
  |Node(gauche,_, droite) -> 1+taille gauche + taille droite;;
  
(*remarque: la taille d'un flexagone est impaire car c'est un arbre binaire entier*)
let rec ordre f = (taille f +3)/2;;
(*l'ordre o et la taille t d'un flexagone vérifient
2*o = t+3
 *)

  

(*met à jour les étiquettes d'un flexagone en étiquettant la racine avec k*)  
let rec set_leaves_aux f k = match f with
  |Leaf(a) -> Leaf(k)
  |Node(gauche,x,droite) -> Node(set_leaves_aux gauche k,k, set_leaves_aux droite (k+1));;

(*numérote correctement les étiquettes d'un flexagone : la racine est à 0*)
let set_leaves f = set_leaves_aux f 0;;


(*concatène un couple de flexagones (f1,f2) sur la feuiile n°k dans le sens horaire si f est correctement numéroté*)
let rec concat_flex_aux f0 k (f1,f2) =
  match f0 with
  |Leaf(a) -> Node(f1,a,f2)
  |Node(gauche,x,droite) -> let og = ordre gauche in
			    if og <= k then Node(concat_flex gauche k (f1,f2),x,droite)
			    else Node(gauche,x,concat_flex droite (k-og) (f1,f2));;

let concat_flex f k (f1,f2) = set_leaves (concat_flex_aux f k (f1,f2));; 


let ajoute_face f k = concat_flex f k (Leaf(0),Leaf(0));;



let rec fais_tourner f k = match f with
  |Node(Leaf(a),b,Leaf(c)) -> if k=a then Node(Leaf(c),a,Leaf(b))
			      else Node(Leaf(a),b,Leaf(c))
  |Node(gauche,x,droite) -> let og = ordre gauche in
			    if k <= og then
			      let part = fais_tourner gauche k in
			      let opart = ordre part in
			      concat_flex part opart (droite,Leaf(x))
			    else
			      let part = fais_tourner droite (k-og) in
			      concat_flex part 1 (Leaf(x),gauche);;

  

let root t = match t with
  |Node(_,a,_) -> a
  |Leaf(a) -> a;;



let rec gauche_droite b = match b with
  |Leaf(a) -> a
  |Node(g,_,d) ->gauche_droite g @ gauche_droite d;;

let bords b = let r = root b in
              let t = gauche_droite b in
              r::t;;


