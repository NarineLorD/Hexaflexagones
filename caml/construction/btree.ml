type 'a btree = 
  |Vide
  |Node of 'a btree*'a*'a btree;;

(*Tout d'abord des fonctions générales sur les arbres binaire*)
let est_vide ab = ab=Vide;;

let rec taille ab = match ab with
  |Vide -> 0
  |Node(g,_,d) -> 1 + taille g + taille d;;

let rec hauteur ab = match ab with
  |Vide -> 0
  |Node(g,_,d) -> 1 + (max (hauteur g) (hauteur d));;

let racine ab = match ab with
  |Vide -> failwith"Pas de racine: cet arbre est vide"
  |Node(_,x,_) -> x;;

let infixe ab = 
  let rec infixe_aux a l = match a with
    |Vide -> l
    |Node(g,x,d) -> infixe_aux g (x::infixe_aux d l)
  in
  infixe_aux ab [];;

let prefixe ab = 
  let rec prefixe_aux ab l = match ab with
    |Vide -> l
    |Node(g,x,d) -> x::(prefixe_aux g (prefixe_aux d l))
  in
  prefixe_aux ab [];;

let postfixe ab = 
  let rec postfixe_aux a l = match a with
    |Vide -> l
    |Node(g,x,d) -> postfixe_aux d (postfixe_aux g (x::l)) 
  in
  postfixe_aux ab [];;

let rec map ab f = match ab with
  |Vide -> Vide
  |Node(g,x,d) -> Node(map g f, f x, map d f);;

let rec for_all f ab = match ab with
  |Vide -> true
  |Node(g,x,d) -> f x && for_all f g && for_all f d;;

(*Le module des arbres binaires de recherche*)
module Abr = struct
  type 'a abr = 'a btree
  let empty () = Vide
  let rec est_dans abr x = match abr with
    |Vide -> false
    |Node(g,y,d) -> x=y || (x<y && est_dans g x) || (x>y && est_dans d x)
  let rec min abr = match abr with
    |Vide -> failwith"Pas de min: cet abre de recherche est vide"
    |Node(Vide,x,_) -> x
    |Node(g,_,_) ->min g
  let rec max abr = match abr with
    |Vide -> failwith"Pas de max: cet arbre est vide"
    |Node(_,x,Vide) -> x
    |Node(_,_,d) -> max d
  let rec add ab x = match ab with
    |Vide -> Node(Vide,x,Vide)
    |Node(g,y,d) -> if x<y then Node(add g x,y,d) 
                else Node(g,y,add d x)
  let rec merge a b = match a with
    |Vide -> b
    |Node(g,x,d) -> let t = merge (merge b d) g in
                    add t x
  let rec map abr f = match abr with
    |Vide -> Vide
    |Node(g,x,d) -> add (merge (map g f) (map d f)) (f x)
end



(*Quelques fonctions sur les arbres dont les noeuds sont des couples:
utilisés pour manipuler les flexagones*)


