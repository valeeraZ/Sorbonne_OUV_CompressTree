open Abr;;

print_string "Test 3.14 3.15: écrire des données d'expérimentations dans les fichiers du directoire /data. Ces expérimentations peuvent durer quelques minutes.";
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
    let e_abr = (sizeof abr) and e_abr_comp = (sizeof abrc) in
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

let file_abr_espace_petit = "data/abr_espace_petit";;
let file_abrcomp_espace_petit = "data/abrcomp_espace_petit";;
let list_n_petit = [50;100;150;200;250;300;350;400;450;500;];;

let c1_petit = (open_out file_abr_espace_petit);;
let c2_petit = (open_out file_abrcomp_espace_petit);;

let rec test_noeud_nb_petit (l: int list) = 
  match l with
  | n::ns -> 
    let list = (gen_permutation n) in
    let abr =  (construction list) in
    let abrc = (construction_comp abr) in
    let e_abr = (sizeof abr) and e_abr_comp = (sizeof abrc) in
    begin
      Printf.fprintf c1_petit "%d %d\n" n e_abr;
      Printf.fprintf c2_petit "%d %d\n" n e_abr_comp;
      test_noeud_nb_petit ns;
    end
  | _ -> 
    begin
      close_out c1_petit;
      close_out c2_petit;
      ();
    end;;

test_noeud_nb_petit list_n_petit;;