(*

Implémentation de triangulations des polygones réguliers

 *)

type triangulation = {sommets: int ref list; aretes: (int ref ref *int ref ref) list; ordre:int}


let (t3: unit -> triangulation) = fun () ->{sommets = [ref 0; ref 1; ref 2]; aretes=[];ordre=3}

let ordre f =  f.ordre
  

let copie_triangulation f = 
  (*ne fonctionne que si les sommets sont étiquettés avec des entiers < ordre f *)
  let n = ordre f in
  let t = Array.make n (ref 0) in
  let rec copie_sommet s = match s with
    |[] -> []
    |x::r -> let a = ref (!x) in 
             t.(!x) <- a; 
             a::(copie_sommet r)
  in
  let s = copie_sommet f.sommets in
  let rec copie_aretes a = match a with
    |[] -> []
    |(x,y)::r -> (ref t.(!(!x)), ref t.(!(!y)))::(copie_aretes r)
  in
  {sommets=s; aretes=copie_aretes f.aretes; ordre=n}



let recolorie f t =
  List.iteri (fun k x -> x:=t.(k)) f.sommets

let rec retrieve x l = match l with
  |[] -> failwith "pas trouvé ce qu'il fallait"
  |a::r -> if x=(!a) then a else retrieve x r 

let ajoute_face f (x,y) =
  let n = ordre f in
  let g = copie_triangulation f in
  if (y mod n = (x+1) mod n && y<=n) then
    let t = Array.init n (fun i -> i) in
    begin
      for i = n-1 downto y do t.(i) <- i+1 done;
      recolorie g t;
      let a,b = retrieve x g.sommets, retrieve (y+1) g.sommets in
      {sommets = (ref y)::g.sommets;aretes = (ref a,ref b)::g.aretes; ordre=n+1}
    end
  else failwith "pas possible d'ajouter une face ici"
       
  

let arrange l =   List.iter (fun (x,y) -> let t = !x in
                          x:= min (!x) (!y); y:= max (t) (!y)) l


let rotation f k = 
  let n = ordre f in
  let g = copie_triangulation f in
  List.iter (fun x -> x := (!x + k) mod n) g.sommets;
  arrange g.aretes; 
  g

let symetrie f k = 
  let n = ordre f in
  let g = copie_triangulation f in
  List.iter (fun x -> x := ((2*(n-k)-1-(!x)) mod n)) g.sommets;
  arrange g.aretes;
  g



let egal f g = 
  (ordre f = ordre g) && List.for_all (fun x -> List.mem x f.aretes)  g.aretes



let orbite t = 
  let n = ordre t in
  let liste = ref [] in
  for i = 0 to n-1 do
    liste := (rotation t i)::(symetrie t i)::(!liste)
  done;
  !liste

let equiv f g = 
  let n = ordre f in
  List.exists (egal f) (orbite g)
                                 

let check_rotation f k = 
  egal (rotation f k) f

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

