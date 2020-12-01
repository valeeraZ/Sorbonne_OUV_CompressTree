open Abr;;

(*Test 1.1*)
let l = [1;2;3;4;5] in
let ea = extraction_alea l [] in
print_string "Test 1.1: ";
print_string "Liste L: ";
print_list (fst ea);
print_string " Liste P:";
print_list (snd ea);
print_newline ();
print_newline ();;

(*Test 1.2*)
print_string "Test 1.2: ";
print_string "Liste P: ";
print_list (gen_permutation 5);
print_newline ();
print_newline ();;

(*Test 1.7*)
print_string "Test 1.7: ";
print_abr (construction [4; 2; 3; 8; 1; 9; 6; 7; 5]);
print_newline ();
print_newline ();;
(*Expect: ( 4 ( 2 ( 1 ε ε ) ( 3 ε ε ) ) ( 8 ( 6 ( 5 ε ε ) ( 7 ε ε ) ) ( 9 ε ε ) ) ) *)

(*Test 2.8*)
print_string "Test 2.8: ";
print_string (phi (construction [4; 2; 3; 8; 1; 9; 6; 7; 5]));
print_newline ();
print_newline ();;

(*Test 2.9*)
print_string "Test 2.9: ";
print_list (prefixe (construction [4; 2; 3; 8; 1; 9; 6; 7; 5]));
print_newline ();
print_newline ();;
(*Expect: 4 2 1 3 8 6 5 7 9*)

(*Test 2.10*)
print_string "Test 2.10: L'arbre compressé: ";
print_abr_comp (construction_comp (construction [4; 2; 3; 8; 1; 9; 6; 7; 5]));
print_newline();
print_newline();;

(*Test 2.11*)
let a = (construction_comp (construction [4; 2; 3; 8; 1; 9; 6; 7; 5])) in
print_string "Test 2.11: cherche 7 dans l'arbre : ";
Printf.printf "%B" (chercher_comp a 7);
print_newline();
print_newline();;

(*Test 3.13*)
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

print_string "Temps pour chercher abr: ";
print_float (time_chercher chercher abr n );
print_newline();

print_string "Temps pour chercher abr_comp: ";
print_float (time_chercher chercher_comp abr_c n );
print_newline();;