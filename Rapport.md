# Présentation

Dans ce projet, nous cherchons une solution de manipuler un modèle de structure de données arborescence, les Arbres de Binaires de Recherche puis d'en construire une structure compressée suivant une procédure bien particulière.



## Sysnthès de données

### Question 1.1

La fonction `extraction_alea` prend en entrée deux listes d'entiers, notée `l` et `p`. La fonction choisit aléatoirement un entier  `r` entre 1 et  la longueur de liste `l`, puis retourne un couple de listes dont la première est la liste `l ` dans laquelle on a retiré le `r`-ième élément et la deuxième est la liste `p` dans laquelle on a ajouté en tête le `r`-ième élément extrait de L.

```ocaml
let rec remove_at n l = match l with
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n-1) t;;

let extraction_alea (l:int list) (p: int list) : (int list) * (int list) = 
  let length = List.length l in
  let r = (Random.int length) in
  let e = List.nth l r in
  ((remove_at r l), (e :: p));;
```

  La fonction auxiliaire `remove_at` permet de retirer un élément à la position `n` dans une liste `l`.

### Question 1.2

La fonction `gen_permutation` réalise l'algorithme de *shuffle de Fisher-Yates*.

```ocaml
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

```

La fonction auxiliaire `interval` permet de générer une liste des entiers de `n` à `m`. Dans la fonction `gen_permutation`, nous l'utilisons pour générer la liste *L* des entiers de 1 à `n`.

### Question 1.3

En fonction de `n`, nous avons besoin d'appeler au générateur de nombres aléatoires `n` fois pour que la liste *L* soit vide et remplit la liste *P* en appelant `extraction_alea` donc la complexité est $O(n)$. 

En nombre de filtrage de motif qui est utilisé dans la fonction `remove_at` de la question 1.1, nous utilisons `match` en parcourant la liste pour trouver l'élément par l'indice donnée par `Random.int`. Supposons cette indice a la valeur `m`, nous avons besoin d'utiliser le filtrage de motif `n * (n-m) `fois donc la complexité est $O(n^2)$.





