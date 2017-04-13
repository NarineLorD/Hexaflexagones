(*
Implémentatio d'un structure de recherche utilisée pour manipuler les triangulations de polygones
Ces boites sont en réalité des ensembles triés sur lesquels on peut faire les opérations classiques
 *)

type 'a ens = 'a list

type 'a boite = {taille:int;
                 content: 'a ens;}


let rec list_it f l b = match l with
  |[] -> b
  |x::r -> f x (list_it f r b)

let rec boite_it f l b = match l.content with
  |[] -> b
  |x::r -> f x (boite_it f {taille=l.taille-1;content=r} b)

let (vide: unit -> 'a ens) = fun () -> []
let empty ()= {taille=0; content=vide()}

let taille b = b.taille

let is_empty e = e=[]
let est_vide b = taille b = 0

let rec ajoute e x = match e with
  |[] -> [x]
  |y::r -> if x<y then x::e
           else y::(ajoute r x)
let add b x = {taille = b.taille+1; content = ajoute b.content x}

let rec is_in e x = match e with
  |[] -> false
  |y::r -> y=x || is_in r x 
let est_dans b x = is_in b.content x

let liste_to_boite l = {taille=List.length l; content = List.sort compare l}





let sort boite = {taille=boite.taille; content= List.sort (Pervasives.compare) boite.content}

<<<<<<< HEAD
let rec union a b = match a.content,b.content with
  |[],_ -> b
  |_,[] -> a
  |x::r,y::l -> if x=y then add (union {taille=a.taille-1;content=r} {taille=b.taille-1;content=l}) x
                else if x<y then add (union {taille=a.taille-1;content=r} b) x
                else add (union a {taille=b.taille-1;content=l}) y

let exists f b = List.exists f b.content
           

(*
keep: ('a -> bool) -> 'a boite -> 'a boite
keep p b, renvoie la boite contenant les éléments de b qui vérifient p*)
let rec keep p b = match b.content with
  |[] -> {taille=0; content=[]}
  |x::r -> let t = keep p {taille=b.taille-1;content=r} in
           if p x then add t x else t
  
let rec map_keep p f b  = match b.content with
  |[] -> {taille=0;content= []}
  |x::r -> let t = map_keep p f ({taille=b.taille-1;content=r}) in
           if p x then add t (f x)
           else t


let boite_to_array b = 
  let n = taille b in
  if n = 0 then [||]
  else let x = List.hd b.content in
       let t = Array.make n x in
       let rec copie l k = match l with
         |[] -> ()
         |x::r -> t.(k) <- x; copie r (k+1);
       in
       copie b.content 0;t

let egalite x y = x=y
let rec clean ?(comp = egalite) b = match b.content with
  |[] -> empty()
  |x::r -> let c = clean ~comp:comp {taille=b.taille-1; content=r} in
           if exists (comp x) c then c
           else add c x


(*
Structure d'ensembles impérative, 
les tables de hachage permettent des opérations de recherche plus rapide
 *)
module Ens = struct
  type 'a ensemble = ('a, unit) Hashtbl.t
  let (vide : int -> 'a ensemble) =
    fun n -> Hashtbl.create n
  let (add : 'a ensemble -> 'a -> unit) =
    fun e x -> Hashtbl.add e x ()
  let (mem : 'a ensemble-> 'a -> bool) =
    fun e x -> Hashtbl.mem e x
