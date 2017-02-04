(*
Ce script doit permettre de construire les représentations de flexagones par les
triangulations de polygones
 *)
open Flexagones
open Arbresb

(*classe f renvoie une liste contenant la classe d'équivalence du flexagone f sous l'action du groupe diédral
ie tous les flexagones obtenus par rotations & reflexions de f*)
let classe f =
  let n = ordre f in
  let g = symetrie f in
  let rec classes_aux f k = match k with
    |0 -> [f;g]
    |k -> (rotation f k)::(rotation g k)::(classes_aux f (k-1)) in
  classes_aux f (n-1);;


(*Pour un flexagone f,
ordre_sup_naif renvoie un arbre binaire de recherche contenant tous flexagones que 
l'on peut obtenir en ajoutant une face à f.
L'algorithme utilisé ici est naif et ne tire pas parti des invariances de f pour éviter
les redondances dans la liste obtenue
 *)

let ordre_sup_naif f = 
  let n = ordre f in
  let l = ref (Abr.empty ()) in
  for i=n downto 1 do
    let x = ajoute_face f (i mod n, i-1) in
    l := Abr.add !l x;
  done;
  !l;;






