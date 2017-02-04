type 'a btree
val est_vide: 'a btree -> bool
val taille: 'a btree -> int
val hauteur: 'a btree -> int
val racine:  'a btree -> 'a
val infixe: 'a btree -> 'a list
val prefixe: 'a btree -> 'a list
val postfixe: 'a btree -> 'a list
val map: 'a btree -> ('a -> 'b) -> 'b btree

module Abr:sig
  type 'a abr = 'a btree
  val empty: unit -> 'a abr
  val est_dans: 'a abr -> 'a -> bool
  val min: 'a abr -> 'a
  val max: 'a abr -> 'a
  val add: 'a abr -> 'a -> 'a abr
end
