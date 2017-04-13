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



let recolorie f t = 
  map (fun (a,b) -> (min t.(a) t.(b), max t.(a) t.(b))) f 


let renomme_sommet f x y = 
(* pour une triangulation f, deux entiers x  et y, si x est le nom d'une face de f (un sommet de la tiangulation),
alors cette fonction doit renvoyer la triangulation g où x a été renommé en y.
Remarque: je ne me soucie pas de savoir si la couleur y est déjà utilisée dans f, faire attention à l'usage, une telle utilisation renvoyerais un graphe non isomorphe au premier!*)
  let g (e,f) = if e=x && f=x then (y,y)
                else if e=x then (min f y,max y f)
                else if f=x then (min y e,max e y)
                else (e,f) in
  map g f


let ajoute_face f (x,y) =
  let n = ordre f in
  if (y mod n = (x+1) mod n && y<=n) then
    let t = Array.init n (fun i -> i) in
    for i = n-1 downto y do t.(i) <- i+1 done;
    let g = recolorie f t in
    let b = (y+1) mod (n+1) in
    add g (min (x ,b) (b,x))
  else failwith "pas possible d'ajouter une face ici"
      
  
  
let rotation t k = 
  let n = ordre t in
  let plus (x,y) = let a,b = (x+k) mod n, (y+k) mod n in (min a b, max a b) in
  map plus f

let symetrie t = 
  let n = ordre t in
  let sym (x,y) = (n-1-y, n-1-x) in
  map sym f

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

let check_symetrie f k = 
  let n = ordre f in
  for_all (est_dans f) (map (fun (x,y) -> let a ,b = ((n-y-k-k) mod n,(n-x-k-k) mod n) in (min a b, max a b)) f)


(*les quelques fonctions suivantes servent à construire l'intégralité des triangulations existantes pour un ordre donné*)

let rec catalan n = 
  if n=0 then 1
  else let c = catalan (n-1) in
       2*(2*n-1)*c/(n+1)

let nombre_triangulation n = 
  assert(n>=3);
  catalan (n-2)




let triangu_sup t = 
  let n = ordre t in
  let b = ref (empty()) in
  for i=0 to n-1 do
    b := add !b (ajoute_face t (i,i+1))
  done;
  clean ~comp:egal !b



let rec triangulations n = 
(*calcule les triangulations du n-gone et les renvoie dans une boite*)
  assert (n>=3);
  if n=3 then add (empty()) (empty())
  else 
    boite_it (fun x y -> union x y) (map triangu_sup (triangulations (n-1))) (empty())

let flexagones n = 
  clean ~comp:equiv (triangulations n)


(*
La suite du module sert à compter le nombre d'arêtes en commun pour deux triangulations quelconques de même ordre

Implémentation du parcours des graphes triangulations:
******* Distance d'édition entre triangulations ******
*)


let nombre_aretes_communes t1 t2 = 
(* Pour deux triangulations t1 et t2, renvoie le nombre d'aretes que ces triangulations ont en commun*)
  taille (intersection t1 t2)
