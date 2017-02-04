(*
Ce script doit permettre de construire les représentations de flexagones par les
triangulations de polygones
 *)

open Flexagones


let classe f =
  let n = ordre f in
  let g = symetrie f in
  let rec classes_aux f k = match k with
    |0 -> [f;g]
    |k -> (rotation f k)::(rotation g k)::(classes_aux f (k-1)) in
  classes_aux f (n-1);;

let rec appartient x l = match l with
  |[] -> false
  |y::r -> x=y || appartient x r;;


let equiv_naif f g = appartient f (classe g);;

let check_rotation f k = 
  let n = ordre f in
  




