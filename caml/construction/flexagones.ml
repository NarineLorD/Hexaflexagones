(*
Idée d'alban pour représenter les triangulations de flexagones:
---L'ordre N (int: 2<N<infnty)
---Les arrêtes internes du flexagones (int*int) list
C'est ce que j'essaye d'implémenter ici
 *)


type flexagone = {ordre:int;
                  aretes:(int*int) list};;


let flexagone_trois = {ordre=3; aretes=[]};;


let ordre f = f.ordre;;

let ajoute_face f a b = 
  let n = ordre f in
  assert (0<= a && 0<= b && a<n && b<n);
  {ordre = f.ordre+1; aretes = (a,b)::f.aretes};;


let rotation f k = 
  let n = ordre f in
  let rec tourne_aux l = match l with
    |[] -> []
    |(x,y)::r -> ((x+k) mod n, (y+k) mod n)::(tourne_aux r) in
  {ordre = n; aretes = tourne_aux f.aretes};;

let symetrie f = 
  let n = ordre f in
  let rec retourne_aux l = match l with
    |[] -> []
    |(x,y)::r -> (n-x, n-y)::(retourne_aux r) in
  {ordre=n; aretes=retourne_aux f.aretes};;

