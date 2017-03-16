type 'a boite

val empty: unit -> 'a boite
val est_vide: 'a boite -> bool
val add: 'a boite -> 'a -> 'a boite
val est_dans: 'a boite -> 'a -> bool
val taille: 'a boite -> int
val map: 'a boite -> ('a -> 'b) -> 'b boite
val for_all: ('a -> bool) -> 'a boite -> bool

val intersection: 'a boite -> 'a boite -> 'a boite
