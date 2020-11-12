(*indexes start at 0*)
let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n-1) t;;

(*Question 1.1 *)
let extraction_alea l p = 
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

let rec interval n m = 
  if n = m 
    then [m]
  else n::interval (n+1) m;;

let gen_permutation n = 
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