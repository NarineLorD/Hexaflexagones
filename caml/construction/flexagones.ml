(*
Idée d'alban pour représenter les triangulations de flexagones:
---L'ordre N (int: 2<N<infnty)
---Les arrêtes internes du flexagones (int*int) list
C'est ce que j'essaye d'implémenter ici
 *)
open Arbresb

type flexagone = {ordre:int;
                  aretes:(int*int) Abr.abr};;


let flexagone_trois = {ordre=3; aretes=Abr.empty ()};;


let ordre f = f.ordre;;

let ajoute_face f (a,b) = 
  let n = ordre f in
  assert (0<= a && 0<= b && a<n && b<n);
  {ordre = f.ordre+1; aretes = Abr.add f.aretes (a,b)};;


let rotation f k = 
  let n = ordre f in
  let plus (x,y) = ((x+k) mod n, (y+k) mod n) in
  {ordre = n; aretes = map f.aretes plus};;

let symetrie f = 
  let n = ordre f in
  let sym (x,y) = (n-x, n-y) in
  {ordre=n; aretes=map f.aretes sym};;

let equiv f g = 
  (ordre f = ordre g) && for_all (Abr.est_dans f.aretes) g.aretes);;

