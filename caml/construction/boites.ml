(*
Implémentatio d'un structure de recherche utilisée pour manipuler les triangulations de polygones
 *)


type 'a boite = {taille:int;
                 content: 'a list;}



let add boite x = {taille = boite.taille+1; content = x::(boite.content)}

let taille boite = boite.taille

let est_dans boite x = 
  let rec est_dans_liste l e = match l with
    |[] -> false
    |y::r -> e=y || est_dans_liste r e
  in
  est_dans_liste boite.content x

let empty () = {taille=0; content = []}

let est_vide boite = boite.taille = 0

let map boite f = 
  {taille=boite.taille;content=List.map f boite. content}
    
let for_all f boite = 
  List.for_all f boite.content 



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
                        
