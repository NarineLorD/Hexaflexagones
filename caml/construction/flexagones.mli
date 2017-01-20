(*
Ce fichier doit contenir les fonctions permettant de manipuler les représentations des flexagones adaptées aux algorithmes de construction
ie les graphes d'adjacence.
Ici sont les opérations de base autorisées

Ces fonctions sont là pour une implantation persistante.
*)
module Flexagones:
sig 
  type flexagone
         
  (*
Représente le flexagone d'ordre 3 à partir duquel on construit les autres
   *)
  val flexagone_trois : flexagone
                          
                          
  (*
Pour un flexagone f, ordre f est le nombre de faces du flexagone
   *)
  val ordre: flexagone -> int
                            
                            
  (*
Pour chaque flexagone f d'ordre n, il est possible d'ajouter un face 
à exactement n positions différentes.
Ces positions sont ordonnées, et ajoute_face flexagone k renvoie le 
flexagone obtenu en ajoutant une face à la kième position disponible.
   *)
  val ajoute_face : flexagone -> k -> flexagone
                                        
                                        
  (*
Pour un entier k et un flexagone f, rotation k f renvoie f "trouné" de 2*k*pi/n rad
   *)
  val tourne: int -> flexagone -> flexagone
                                    
  (*
retourne f renvoie le symetrique du flexagone f
   *)
  val retourne: flexagone -> flexagone
                               
end
