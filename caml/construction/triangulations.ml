(*

Implémentation de triangulations des polygones réguliers

 *)
open Boites

type triangulation = (int*int) boite


let (t3:unit -> triangulation) = fun () -> empty ()


let ordre t = taille t + 3

let ajoute_face_naif t (x,y) = 
  let a,b = min x y, max x y in
  add t (a,b)
(*en pratique il faut renommer O(n) sommets pour faire des choses correctes par la suite*)




let renomme_sommet t x y = 
(* pour une triangulation t, deux entiers x  et y, si x est le nom d'un sommet de t,
alors cette fonction doit renvoyer la triangulation g où x a été renommé en y.
Remarque: je ne me soucie pas de savoir si la couleur y est déjà utilisée dans t, faire attention à l'usage !*)
  let modif (a,b) = if a=x && b=x then (y,y)
                else if a=x then (min b y,max y b)
                else if b=x then (min y a,max a y)
                else (a,b) in
  map t modif


let ajoute_face t (x,y) =
  let n = ordre t in
  assert (y mod n = (x+1) mod n && y<n);
  let g = ref t in
  for i = n-1 downto max x y do
    g := renomme_sommet !g i (i+1)
  done;
  if x<y then add !g (x,y+1)
  else add !g (y+1,x+1)

  
  
let rotation t k = 
  let n = ordre t in
  let plus (x,y) = let a,b = (x+k) mod n, (y+k) mod n in (min a b, max a b) in
  sort (map t plus)

let symetrie t = 
  let n = ordre t in
  let sym (x,y) = (n-1-y, n-1-x) in
  sort (map t sym)

let egal t u = 
  (ordre t = ordre u) && for_all (est_dans t)  u

let equiv t u = 
  let n = ordre t in
  let test = ref (egal t u) in
  let u' = ref u in
  for i = 0 to n do
    u' := rotation !u' 1;
    test := !test || egal !u' t;
  done;
  u' := symetrie !u';
  for i = 0 to n do
    test := !test || egal !u' t;
    u' := rotation !u' 1;
  done;
  !test
                                 

let check_rotation t k = 
  egal (rotation t k) t

let check_symetrie t k = 
  let n = ordre t in
  for_all (est_dans t) (map t (fun (x,y) -> let a ,b = ((n-y-k-k) mod n,(n-x-k-k) mod n) in (min a b, max a b)))






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
