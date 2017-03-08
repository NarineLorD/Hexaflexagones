(*
Idée d'alban pour représenter les triangulations de flexagones:
---L'ordre N (int: 2<N<infnty)
---Les arrêtes internes du flexagones (int*int) list
C'est ce que j'essaye d'implémenter ici
 *)
open Btree


type flexagone = {ordre:int;
                  aretes:(int*int) Abr.abr}


let f3 () = {ordre=3; aretes=Abr.empty ()}


let ordre f = f.ordre

let ajoute_face_naif f (x,y) = 
  let a,b = min x y, max x y in
  {ordre = f.ordre+1; aretes = Abr.add f.aretes (a,b)}
(*en pratique il faut renommer O(n) sommets pour faire des choses correctes par la suite*)


let change_couleur_flexagone f x y = 
(* pour un flexagone f, deux entiers x  et y, si x est le nom d'une face de f (un sommet de la tiangulation),
alors cette fonction doit renvoyer le flexagone g où x a été renommé en y.
On pourrait appeler cette fonction change_couleur: change la couleur d'une face.
Remarque: je ne me soucie pas de savoir si la couleur y est déjà utilisée dans f, faire attention à l'usage !*)
  {ordre = ordre f; aretes = change_couleur (f.aretes) x y}


let ajoute_face f (x,y) =
  let n = ordre f in
  assert (y mod n = (x+1) mod n && y<n);
  let g = ref f in
  for i = n-1 downto y do
    g := change_couleur_flexagone !g i (i+1)
  done;
  {ordre = n+1; aretes = Abr.add (!g.aretes) (x,y+1)}

  
  
let rotation f k = 
  let n = ordre f in
  let plus (x,y) = let a,b = (x+k) mod n, (y+k) mod n in (min a b, max a b) in
  {ordre = n; aretes = map f.aretes plus}

let symetrie f = 
  let n = ordre f in
  let sym (x,y) = (n-1-y, n-1-x) in
  {ordre=n; aretes=map f.aretes sym}



let egal f g = 
  (ordre f = ordre g) && for_all (Abr.est_dans f.aretes)  g.aretes

let equiv f g = 
  let n = ordre f in
  let t = ref (egal f g) in
  let h = ref g in
  for i = 0 to n do
    h := rotation !h 1;
    t := !t || egal !h f;
  done;
  h := symetrie !h;
  for i = 0 to n do
    t := !t || egal !h f;
    h := rotation !h 1;
  done;
  !t
                                 

let check_rotation f k = 
  egal (rotation f k) f

let check_symetrie f k = 
  let n = ordre f in
  for_all (Abr.est_dans f.aretes) (map f.aretes (fun (x,y) -> let a ,b = ((n-y-k-k) mod n,(n-x-k-k) mod n) in (min a b, max a b)));;
  
  
  
let f4 () = ajoute_face (f3()) (0,1);;

let a = ref (f4());;
let b = ref (f4());;
a := ajoute_face !a (0,1);;
b := ajoute_face !b (1,2);;
equiv !a !b;;
