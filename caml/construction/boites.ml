(*
Implémentation d'un structure de recherche utilisée pour manipuler les triangulations de polygones
 *)

type 'a boite = 
  {
    taille:int; (*nombre d'élément*)
    content: 'a list (*structure où sont stockés les éléments, ici une liste*);
  }


let empty () = {taille=0; content = []}

let taille boite = boite.taille

let est_vide boite = boite.taille = 0

let rec add boite x = match boite.content with
  |[] -> {taille=1;content=[x]}
  |y::r -> let n = boite.taille in
           if x<=y then {taille=n+1; content=(x::y::r)}
           else add (add {taille=n-1;content=r} x) y

let rec est_dans boite x = 
  match boite.content with
  |[] -> false
  |y::r -> x=y || ( x>y && est_dans {taille=boite.taille-1;content=r} x)

let map boite f = 
  {taille=boite.taille;content=List.map f boite. content}

let for_all f boite = 
  List.for_all f boite.content


let sort boite = {taille=boite.taille; content= List.sort (Pervasives.compare) boite.content}

(* mauvaise implémentation

let rec intersection a b = 
(*Pour deux boites a et b, renvoie une boite contenant les éléments qui sont dans a et dans b*)
  match a.content,b.content with
  |[],_ -> {taille=0;content=[]}
  |_,[] -> {taille=0;content=[]}
  |x::r, y::q -> let n,m = taille a, taille b in 
                 let g = intersection {taille = n-1;content=r} {taille = m-1;content=q} in
                 let t = taille g in
                 if x=y then {taille=t+1; content=x::g.content}
                 else {taille=t; content=g.content} 
 *)       
