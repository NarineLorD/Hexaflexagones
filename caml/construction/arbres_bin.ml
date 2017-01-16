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

(*type step
Indique une direction à prendre sur le noeud d'un arbre binaire:
        true  <=> gauche
        false <=> droite
*)
type step = bool;;


let root t = match t with
  |Node(_,a,_) -> a
  |Leaf(a) -> a;;



let rec gauche_droite b = match b with
  |Leaf(a) -> a
  |Node(g,_,d) ->gauche_droite g @ gauche_droite d;;

let bords b = let r = root b in
              let t = gauche_droite b in
              r::t;;


