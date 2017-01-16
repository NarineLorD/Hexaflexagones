(*
Premier essai:
implémentation de flexagones par des tableaux.
 *)

let array_rot_unit t =
  let n = Array.length t in
  let a = ref t.(n-1) in
  for i = n-1 downto 1 do
    let a = ref t.(i) in
    t.(i) <- t.(i-1);
  done;
  t.(0) <- !a;;

let array_rot t k =
  for i = 0 to k-1 do
    array_rot_unit t;
  done;;

let swap t i j = let a = ref t.(i) in
		 t.(i) <- t.(j);
		 t.(j) <- !a;;
let array_refl t =
  let n = Array.length t in
  for i = 0 to n/2-1 do
    swap t i (n-i-1);
  done;;




let diedral_equiv t1 t2 =
  let n1,n2 = Array.length t1,Array.length t2 in
  assert (n1=n2);
  let n = n1 in
  let compt1 = ref 0 in
  while !compt1<2*n && t1 <> t2 do
    let compt2 = ref 0 in
    while !compt1 <2*n && !compt2<1 && t1 <> t2 do
      array_refl t2;
      incr compt1;
      incr compt2;
    done;
    array_rot_unit t2;
    incr compt1;
  done;
  (!compt1=2*n);;

