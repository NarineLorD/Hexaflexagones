(*
Ce script doit permettre de construire les représentations de flexagones par les
triangulations de polygones
 *)
open Flexagones

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


(*On va utiliser une table de hachage pour construire rapidement la liste des nouveaux flexagones
ajouter une face est quasi-instantané, le problème est de savoir si le nouveau est déjà construit
avec les tables de hachage on fait ça sans problème.
Je ne me fatigue pas à les implémenter, j'utilise la bibliothèque Ocaml fournie*)







