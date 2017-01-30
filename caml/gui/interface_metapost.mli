(*
Je souhaite utiliser Ocaml pour écrire des fichiers metapost affichant les patrons de flexagones.
Ce fichier contient les valeurs des fonctions permettant cette écriture
 *)


(*type des points graphiques*)
type point;;

(*permet de définir l'orientation d'une figure*)
type orientation;;


val cree_fichier : string -> unit;;

val triangle_equi : point -> orientation -> unit;;




