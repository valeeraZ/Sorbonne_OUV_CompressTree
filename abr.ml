(*indexes start at 0*)
let rec print_list = function 
    [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let rec remove_at n l = match l with
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

(*Question 2.10*)
type abr_comp = 
  | VideComp
  | NoeudComp of {etq: int; fg: abr_comp; fd: abr_comp; }
  | Pointeur of {etqs: int list; point: abr_comp ref};;

(*print un arbre sous référence en suivant racine - fils gauche - fils droit*)
let rec print_abr_comp_ref (a: abr_comp ref) = match !a with
  | VideComp -> print_string "ε "
  | NoeudComp(n) -> 
    print_string "( ";
    print_int n.etq;
    print_string " ";
    print_abr_comp_ref (ref n.fg);
    print_abr_comp_ref (ref n.fd);
    print_string ") ";
  | Pointeur(n) -> 
    print_list n.etqs;
    print_abr_comp_ref n.point;;

(*print un arbre en suivant racine - fils gauche - fils droit*)
let rec print_abr_comp (a: abr_comp) = match a with
  | VideComp -> print_string "ε "
  | NoeudComp(n) -> 
    print_string "( ";
    print_int n.etq;
    print_string " ";
    print_abr_comp n.fg;
    print_abr_comp n.fd;
    print_string ") ";
  | Pointeur(n) -> 
    print_string "[ ";
    print_list n.etqs;
    print_string "]";
    print_string "->";
    print_abr_comp_ref n.point;;

(*fonction phi pour abr_comp*)
let rec phi_comp (a: abr_comp) : string = match a with
  | VideComp -> ""
  | NoeudComp(n) -> "(" ^ (phi_comp n.fg) ^ ")" ^ (phi_comp n.fd)
  | Pointeur(n) -> "";;

(*fonction prefixe pour abr_comp*)
let rec prefixe_comp (a: abr_comp) : int list = match a with
  | VideComp -> []
  | NoeudComp(n) -> (n.etq)::(prefixe_comp n.fg)@(prefixe_comp n.fd)
  | Pointeur(n) -> n.etqs;;

(*égalité des structures de deux arbres comp*)
let egal_structure (a1: abr_comp) (a2: abr_comp) : bool = match (a1, a2) with
  |(VideComp,_) -> false
  |(_,VideComp) -> false
  |(_,_) -> (phi_comp a1) = (phi_comp a2)

(*identité de deux arbres comp*)
let identique (a1: abr_comp) (a2: abr_comp) : bool = 
  (prefixe_comp a1) = (prefixe_comp a2)

(*chercher un arbre comp dans une liste ayant le même structure*)
let rec find (a: abr_comp) (l: abr_comp ref list) : (abr_comp ref) =
  match l with
  | [] -> (ref VideComp)
  | x::xs -> if (egal_structure a !x)  then x else (find a xs)

(*existence d'un arbre identique dans une liste*)
let rec exist_identique (a: abr_comp) (l: abr_comp ref list) : bool =
  match l with
  | [] -> false
  | x::xs -> if (identique a !x) then true else (exist_identique a xs)

(*existence d'un arbre ayant le même structure dans une liste*)
let rec exist (a: abr_comp) (l: abr_comp ref list) =
  match l with
  | [] -> false
  | x::xs -> if (egal_structure a !x) then true else (exist a xs)

(*tous les arbres: 9 en total pour l'exemple en énoncé*)
let rec arbres (a: abr_comp) : (abr_comp ref list) = match a with
  | VideComp -> []
  | NoeudComp(n) -> (arbres n.fg)@(arbres n.fd)@[(ref a)]
  | Pointeur(n) -> [];;

(*trouver tous les différents structures: 4 en total pour l'exemple en énoncé*)
let diff_sous_arbres (a: abr_comp) : (abr_comp ref list) = 
  let rec aux (l1: abr_comp ref list) (l2: abr_comp ref list) = 
    match l1 with
    | [] -> l2
    | x::xs -> if(exist !x l2) then (aux xs l2)
      else (aux xs (x::l2))
  in (aux (arbres a) []);;

(*initilisation sans pointeur pour construction d'un arbre comp à partir d'un arbre orginal*)
let rec init (a: abr) : abr_comp = match a with
  | Vide -> VideComp
  | Noeud(x) -> NoeudComp {etq = x.etq; fg = (init x.fg); fd = (init x.fd)};;

let rec construction_comp (a: abr) : abr_comp = match a with
  | Vide -> VideComp
  | Noeud(n) ->
    let ab = (init a) in
    let l = (arbres ab) in
    let rec replace (a: abr_comp) = 
      let e = (find a l) in 
      let flag_found = (VideComp != !e) in
      if flag_found = true then 
        if (identique a !e) = true
        then match a with
          | VideComp -> VideComp
          | NoeudComp(x) -> NoeudComp {etq = x.etq; fg = (replace x.fg ); fd = (replace x.fd )}
          | Pointeur(x) -> Pointeur {etqs = x.etqs; point = x.point}
        else match a with
          | VideComp -> VideComp
          | _ -> Pointeur {etqs = (prefixe_comp a); point = e}
      else
        match a with
        | VideComp -> VideComp
        | NoeudComp(x) -> NoeudComp {etq = x.etq; fg = (replace x.fg ); fd = (replace x.fd )}
        | Pointeur(x) -> Pointeur {etqs = x.etqs; point = x.point}
    in (replace ab );;

print_string "Test 2.10: L'arbre compressé: ";
print_abr_comp (construction_comp (construction [4; 2; 3; 8; 1; 9; 6; 7; 5]));
print_newline();;

(*Question 2.11*)

(*fils gauche d'un abr comp*)
let filsGauche (a: abr_comp ref) : abr_comp ref = match !a with
  | VideComp -> ref VideComp
  | NoeudComp(n) -> ref n.fg
  | Pointeur(n) -> ref VideComp;;

(*fils droit d'un abr comp*)
let filsDroit (a: abr_comp ref) : abr_comp ref = match !a with
  | VideComp -> ref VideComp
  | NoeudComp(n) -> (ref n.fd)
  | Pointeur(n) -> ref VideComp;;

(*taille d'un abr comp ref -> nombre d'éléments dans prefixe*)
let taille_comp (a: abr_comp ref) : int = (List.length (prefixe_comp !a));;

(*une liste contenant les éléments entre i eme et k eme (commence de 0)(i et k sont inclus) éléments de la liste originale*)
let slice list i k =
  let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n-1) t
  in
  let rec drop n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else drop (n-1) t
  in
  take (k - i + 1) (drop i list);;

let rec chercher_comp (a: abr_comp) (e: int) : bool = match a with
  | VideComp -> false
  | NoeudComp(n) -> 
    if (e < n.etq)
    then (chercher_comp n.fg e)
    else
    if (e > n.etq) then (chercher_comp n.fd e)
    else true
  | Pointeur(n) -> 
    let rec chercher_tab (a: abr_comp ref) (t: int list) (e: int) : bool = match t with
      | [] -> false
      | x::xs -> 
        if (e = x)
        then true
        else let size_g = (taille_comp (filsGauche a)) in
          if (e < x) 
          then chercher_tab (filsGauche a) (slice t 1 size_g) e
          else chercher_tab (filsDroit a)  (slice t (size_g + 1) ((List.length t)-1) ) e
    in chercher_tab n.point n.etqs e;;

let rec chercher (a: abr) (e: int) : bool = match a with
  | Vide -> false
  | Noeud(n) ->
    if (e < n.etq)
    then (chercher n.fg e)
    else
    if (e > n.etq) then (chercher n.fd e)
    else true;;

(*Test 2.11*)
let a = (construction_comp (construction [4; 2; 3; 8; 1; 9; 6; 7; 5])) in
print_string "Test 2.11: cherche 7 dans l'arbre : ";
Printf.printf "%B" (chercher_comp a 7);
print_newline();;

(*Question 3.13*)
let time f x : float=
  let t = Sys.time() in
  let fx = f x in
  (*Printf.printf "Execution time: %fs\n" (Sys.time() -. t);*)
  fx;
  (Sys.time() -. t);;

let time_chercher f x y: float =
  let t = Sys.time() in
  let fx = f x y in
  (*Printf.printf "Execution time: %fs\n" (Sys.time() -. t);*)
  fx;
  (Sys.time() -. t);;

let sizeof (x: 'a) : int = 
  Obj.reachable_words(Obj.repr x);;

let size_abr (a: abr) : int = sizeof a;;

let rec size_abr_comp (a: abr_comp) : int = match a with
  | VideComp -> sizeof VideComp
  | NoeudComp(n) -> 4 + (size_abr_comp n.fg) + (size_abr_comp n.fd)
  | Pointeur(n) -> (sizeof n.etqs) + 1 ;;

let n = 100 in
print_string "Test 3.13: complexité en temps et en mémoire avec n = "; print_int n; 
print_newline();

print_string "Temps pour construction abr: ";
let list =  (gen_permutation n) in
print_float  (time construction list);
print_newline();
(*print_float(time construction list1);*)

let abr =  (construction list) in

print_string "Temps pour construction abr_comp: ";
print_float (time construction_comp abr);
print_newline();
(*print_float(time construction_comp cons_abr_c);*)

let abr =  (construction list) in
let abr_c = (construction_comp abr) in
print_string "Espace de structure abr: ";
print_int (size_abr abr);
print_newline();

print_string "Espace de strucuture abr_comp: ";
print_int (size_abr_comp abr_c);
print_newline();

print_string "Temps pour cherche abr : ";
print_float (time_chercher chercher abr 500 );
print_newline();

print_string "Temps pour chercher abr_comp: ";
print_float (time_chercher chercher_comp abr_c 500 );
print_newline();;

let rec somme_temps_chercher_abr (a: abr) (l: int list) = match l with
  | x::xs -> (time_chercher chercher a x) +. (somme_temps_chercher_abr a xs)
  | _ -> 0.;;

let rec somme_temps_chercher_abr_comp (a: abr_comp) (l: int list) = match l with
  | x::xs -> (time_chercher chercher_comp a x) +. (somme_temps_chercher_abr_comp a xs)
  | _ -> 0.;;

let file_chercher_abr_temp = "data/chercher_abr";;
let file_chercher_abrcomp_temp = "data/chercher_abrcomp";;
let file_abr_espace = "data/abr_espace";;
let file_abrcomp_espace = "data/abrcomp_espace";;
let list_n = [500;1000;1500;2000;2500;3000;3500;4000;4500;5000;];;

let c1 = (open_out file_abr_espace);;
let c2 = (open_out file_abrcomp_espace);;
let c3 = (open_out file_chercher_abr_temp);;
let c4 = (open_out file_chercher_abrcomp_temp);;

let rec test_noeud (l: int list) = 
  match l with
  | n::ns -> 
    let list = (gen_permutation n) in
    let abr =  (construction list) in
    let abrc = (construction_comp abr) in
    let e_abr = (size_abr abr) and e_abr_comp = (size_abr_comp abrc) in
    let t_chercher = (somme_temps_chercher_abr abr list) and t_chercher_comp = (somme_temps_chercher_abr_comp abrc list) in
    begin
      Printf.fprintf c1 "%d %d\n" n e_abr;
      Printf.fprintf c2 "%d %d\n" n e_abr_comp;
      Printf.fprintf c3 "%d %f\n" n t_chercher;
      Printf.fprintf c4 "%d %f\n" n t_chercher_comp;
      test_noeud ns;
    end
  | _ -> 
    begin
      close_out c1;
      close_out c2;
      close_out c3;
      close_out c4;
      ();
    end;;

test_noeud list_n;;