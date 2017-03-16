(*interface
structure abstraite des flexagones
 *)

type triangulation
 
val f3: unit -> triangulation
val ordre: triangulation -> int
val ajoute_face: triangulation -> (int*int) -> triangulation
val rotation: triangulation -> int -> triangulation
val symetrie: triangulation -> triangulation
val equiv: triangulation-> triangulation -> bool
val check_rotation: triangulation -> int -> bool
val check_symetrie: triangulation -> int -> bool

