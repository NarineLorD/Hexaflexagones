(*

Implémentation de triangulations des polygones réguliers

 *)
open Boites

type triangulation = (int*int) boite


let f3 () = empty ()


let ordre f = taille f + 3

let ajoute_face_naif f (x,y) = 
  let a,b = min x y, max x y in
  add f (a,b)
(*en pratique il faut renommer O(n) sommets pour faire des choses correctes par la suite*)




let renomme_sommet f x y = 
(* pour une triangulation f, deux entiers x  et y, si x est le nom d'une face de f (un sommet de la tiangulation),
alors cette fonction doit renvoyer le flexagone g où x a été renommé en y.
On pourrait appeler cette fonction change_couleur: change la couleur d'une face.
Remarque: je ne me soucie pas de savoir si la couleur y est déjà utilisée dans f, faire attention à l'usage !*)
  let g (e,f) = if e=x && f=x then (y,y)
                else if e=x then (min f y,max y f)
                else if f=x then (min y e,max e y)
                else (e,f) in
  map f g


let ajoute_face f (x,y) =
  let n = ordre f in
  assert (y mod n = (x+1) mod n && y<n);
  let g = ref f in
  for i = n-1 downto y do
    g := renomme_sommet !g i (i+1)
  done;
  add !g (x,y+1)

  
  
let rotation f k = 
  let n = ordre f in
  let plus (x,y) = let a,b = (x+k) mod n, (y+k) mod n in (min a b, max a b) in
  map f plus

let symetrie f = 
  let n = ordre f in
  let sym (x,y) = (n-1-y, n-1-x) in
  map f sym



let egal f g = 
  (ordre f = ordre g) && for_all (est_dans f)  g

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
  for_all (est_dans f) (map f (fun (x,y) -> let a ,b = ((n-y-k-k) mod n,(n-x-k-k) mod n) in (min a b, max a b)))






(*les quelques fonctions suivantes servent à construire l'intégralité des triangulations existantes pour un ordre donné*)

let rec catalan n = 
  if n=0 then 1
  else let c = catalan (n-1) in
       2*(2*n-1)*c/(n+1)

let nombre_triangulation n = 
  assert(n>=3);
  catalan (n-2)


(*
La suite du module sert à compter le nombre d'arêtes en commun pour deux triangulations quelconques de même ordre*)

