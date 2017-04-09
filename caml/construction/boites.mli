type 'a boite

val empty: unit -> 'a boite
val est_vide: 'a boite -> bool
val add: 'a boite -> 'a -> 'a boite
val est_dans: 'a boite -> 'a -> bool
val taille: 'a boite -> int
val map: ('a -> 'b) -> 'a boite -> 'b boite
val for_all: ('a -> bool) -> 'a boite -> bool
val exists: ('a -> bool) -> 'a boite -> bool
val boite_it: ('a -> 'b -> 'b) -> 'a boite -> 'b -> 'b
val clean: ?comp:('a boite -> 'a boite -> bool) -> 'a boite boite -> 'a boite boite
val keep: ('a -> bool) -> 'a boite -> 'a boite


val intersection: 'a boite -> 'a boite -> 'a boite
val union: 'a boite -> 'a boite -> 'a boite
