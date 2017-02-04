type 'a btree = 
  |Vide
  |Node of 'a btree*'a*'a btree;;


type 'a arbre_rech  =
  {values:'a btree ;
   c: 'a -> int;};;

let ordre_infixe abr = 
  let rec infixe ab = match ab with
    |Vide -> []
    |Node(g,x,d) -> (infixe g)@x::(infixe d)


type 'a arbre_eq = 'a btree;;

let rec appartient x bt = match bt with
  |Vide -> false
  |Node(gauche, y, droite) -> x=y || (x<y && appartient x gauche) || (x>y && appartient x droite);;


let sommet arbre = match arbre with
  |Vide -> failwith"Cet arbre est vide, il n'a pas de sommet"
  |Node(_,x,_) -> x;;



let rec ajouter x bt = match bt with
  |Vide -> Node(Vide,x,Vide)
  |Node(g,y,d) -> if x<y then let nouveau = Node(ajouter


let rec ajouter x bt = match bt with
  |Vide -> Node(Vide,x,Vide)
  |Node(Vide,y,Vide) -> if x<y then Node(Node(Vide,x,Vide),y,Vide) else Node(Vide,y,Node(Vide,x,Vide))
  |Node(g,y, Vide) -> let yg = sommet g and a,b = min x y, max x y in
                      if a < yg then Node(ajouter a g,b,Vide)
                      else Node(g,b,Node(Vide,a,Vide))
  |Node(Vide,y,d) -> let yd = sommet d and a,b = min x y, max x y in
                     if b>yd then Node(Vide, a, ajouter b d)
                     else Node(Node(Vide,a,Vide),b,d)
  |Node(g,y,d) -> if x<y then Node(ajouter x g,y,d)
                  else Node(g,x, ajouter y d);;

