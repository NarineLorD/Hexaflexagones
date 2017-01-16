type 'a btree = 
  |Leaf of 'a
  |Node of ('a btree)*'a*('a btree);;

let root t = match t with
  |Node(_,a,_) -> a
  |Leaf(a) -> a;;



let rec gauche_droite b = match b with
  |Leaf(a) -> a
  |Node(g,_,d) ->gauche_droite g @ gauche_droite d;;

let bords b = let r = root b in
              let t = gauche_droite b in
              r::;t;
