(*interface
structure abstraite des flexagones
 *)

type flexagone
 
val f3: unit -> flexagone
val ordre: flexagone -> int
val ajoute_face: flexagone -> (int*int) -> flexagone
val rotation: flexagone -> int -> flexagone
val symetrie: flexagone -> flexagone
val equiv: flexagone-> flexagone -> bool
val check_rotation: flexagone -> int -> bool
val check_symetrie: flexagone -> int -> bool

