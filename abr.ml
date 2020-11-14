(*indexes start at 0*)
let rec print_list = function 
    [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let rec remove_at (n: int) (l: int list): int list = match l with
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n-1) t;;

(*Question 1.1*)
let extraction_alea (l:int list) (p: int list) : (int list) * (int list) = 
  let length = List.length l in
  let r = (Random.int length) in
  let e = List.nth l r in
  ((remove_at r l), (e :: p));;

(*Test 1.1*)
let l = [1;2;3;4;5] in
let ea = extraction_alea l [] in
print_string "Test 1.1: ";
print_string "Liste L: ";
print_list (fst ea);
print_string " Liste P:";
print_list (snd ea);
print_newline ();;

let rec interval (n: int) (m: int) : int list = 
  if n = m 
  then [m]
  else n::interval (n+1) m;;

let gen_permutation (n: int) : int list = 
  let l = interval 1 n in
  let p = [] in
  let rec shuffle s t = 
    if List.length s = 0 
    then t
    else 
      let ea = (extraction_alea s t) in
      shuffle (fst ea) (snd ea) in
  shuffle l p;;

(*Test 1.2*)
print_string "Test 1.2: ";
print_string "Liste P: ";
print_list (gen_permutation 5);
print_newline ();;

(*Question 1.7*)
type abr = 
  | Noeud of {etq: int; fg: abr; fd: abr}
  | Vide;;

let rec inserer (e: int) (a: abr) : abr = match a with
  | Vide -> Noeud {etq = e; fg = Vide; fd = Vide}
  | Noeud(n) ->
    if (e < n.etq)
    then Noeud {etq = n.etq; fg = (inserer e n.fg); fd = n.fd }
    else Noeud {etq = n.etq; fg = n.fg; fd = (inserer e n.fd) }

let construction (l: int list) = 
  let vide = Vide in
  let rec aux (l: int list) (a: abr) = 
    if l = [] then a
    else aux (List.tl l) (inserer (List.hd l) a)
  in aux l vide;;

let rec print_abr (a: abr) = match a with
  | Vide -> print_string "ε "
  | Noeud(n) -> 
    print_string "( ";
    print_int n.etq;
    print_string " ";
    print_abr n.fg;
    print_abr n.fd;
    print_string ") ";; 

(*Test 1.7*)
print_string "Test 1.7: ";
print_abr (construction [4; 2; 3; 8; 1; 9; 6; 7; 5]);
print_newline ();;
(*Expect: ( 4 ( 2 ( 1 ε ε ) ( 3 ε ε ) ) ( 8 ( 6 ( 5 ε ε ) ( 7 ε ε ) ) ( 9 ε ε ) ) ) *)

(*Question 2.8*)
(*La fonction Ø qui se lit "phi" en Grec*)
let rec phi (a: abr) : string = match a with
  | Vide -> ""
  | Noeud(n) -> "(" ^ (phi n.fg) ^ ")" ^ (phi n.fd);;

(*Test 2.8*)
print_string "Test 2.8: ";
print_string (phi (construction [4; 2; 3; 8; 1; 9; 6; 7; 5]));
print_newline ();;

(*Question 2.9*)
let rec prefixe (a: abr) : int list = match a with
  | Vide -> []
  | Noeud(n) -> (n.etq)::(prefixe n.fg)@(prefixe n.fd);;

(*Test 2.9*)
print_string "Test 2.9: ";
print_list (prefixe (construction [4; 2; 3; 8; 1; 9; 6; 7; 5]));
print_newline ();;
(*Expect: 4 2 1 3 8 6 5 7 9*)