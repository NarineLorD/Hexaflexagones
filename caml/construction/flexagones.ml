(*
Idée d'alban pour représenter les triangulations de flexagones:
---L'ordre N (int: 2<N<infnty)
---Les arrêtes internes du flexagones (int*int) list
C'est ce que j'essaye d'implémenter ici
 *)
open Arbresb

type flexagone = {ordre:int;
                  aretes:(int*int) Abr.abr}


let f3 () = {ordre=3; aretes=Abr.empty ()}


let ordre f = f.ordre

let ajoute_face f (x,y) = 
  let a,b = min x y, max x y in
  {ordre = f.ordre+1; aretes = Abr.add f.aretes (a,b)}

let rotation f k = 
  let n = ordre f in
  let plus (x,y) = ((x+k) mod n, (y+k) mod n) in
  {ordre = n; aretes = map f.aretes plus}

let symetrie f = 
  let n = ordre f in
  let sym (x,y) = (n-x, n-y) in
  {ordre=n; aretes=map f.aretes sym}

let equiv f g = 
(*cette fonction ne fonctionne pas (ne renvoie pas le bon résultat)*)
(*à corriger: ne pas se laisser berner par des permutations de sommet...*)
  (ordre f = ordre g) && for_all (Abr.est_dans f.aretes)  g.aretes


let check_rotation f k = 
  for_all (Abr.est_dans f.aretes) (rotation f k).aretes

let check_symetrie f k = 
  let n = ordre f in
  for_all (Abr.est_dans f.aretes) (map f.aretes (fun (x,y) -> ((n-x-k-k) mod n,(n-x-k-k) mod n)) )



