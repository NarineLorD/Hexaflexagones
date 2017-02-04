(*interface
structure abstraite des flexagones
 *)

type flexagone

val ordre: flexagone -> int
val ajoute_face flexagone -> (int*int) -> flexagone
val rotation: flexagone -> int -> flexagone
val symetrie: flexagone -> flexagone
val equiv: flexagone-> flexagone -> bool


