(*
Implémentatio d'un structure de recherche utilisée pour manipuler les triangulations de polygones
Ces boites sont en réalité des ensembles triés sur lesquels on peut faire les opérations d'intersection et d'union classique
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
  |y::r -> y=x || (y<x && is_in r x) 
let est_dans b x = is_in b.content x

let map_ens f e = List.map f e
let map f b = {taille=taille b; content = map_ens f b.content}

let for_all_ens f e = List.for_all f e
let for_all f b = for_all_ens f b.content


let rec intersection a b = match a.content,b.content with
  |[],_ -> {taille=0;content=[]}
  |_,[] -> {taille=0;content=[]}
  |x::r,y::l -> if x=y then add (intersection {taille= a.taille -1;content=r} {taille=b.taille-1;content=l}) x 
                else if x<y then intersection {taille=a.taille-1;content=r} b
                else intersection a {taille=b.taille-1;content=l}


let rec union a b = match a.content,b.content with
  |[],_ -> b
  |_,[] -> a
  |x::r,y::l -> if x=y then add (union {taille=a.taille-1;content=r} {taille=b.taille-1;content=l}) x
                else if x<y then add (union {taille=a.taille-1;content=r} b) x
                else add (union a {taille=b.taille-1;content=l}) y

let exists f b = List.exists f b.content


let egal x y = x=y
let rec clean ?(comp = egal) b = match b.content with
  |[] -> empty()
  |x::r -> let c = clean ~comp:comp {taille=b.taille-1; content=r} in
           if exists (comp x) c then c
           else add c x
             


