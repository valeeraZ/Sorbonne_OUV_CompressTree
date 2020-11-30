# Présentation

Dans ce projet, nous cherchons une solution de manipuler un modèle de structure de données arborescence, les Arbres de Binaires de Recherche puis d'en construire une structure compressée suivant une procédure bien particulière.

## Synthèse de données

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



## Construction de l'ABR

Un arbre binaire est : 

- soit réduit à une feuille
- soit décomposable en une racine qui est un nœud interne et qui pointe vers deux enfants ordonnés, l’enfant gauche et l’enfant droit.

En prenant ces deux cas, nous avons défini un type `abr` pour présenter un arbre binaire.

```ocaml
type abr = 
  | Noeud of {etq: int; fg: abr; fd: abr}
  | Vide;;
```

`etq` présente l'étiquette sur le noeud, `fg` et `fd` représentent le fils gauche et le fils droit respectivement du noeud.

Pour un arbre binaire de recherche, la racine possède une étiquette plus grande que toutes les étiquettes de l’enfant gauche, et plus petite que toutes les étiquettes de l’enfant droit. 

Étant donnée une liste d'entiers, nous construisons l'ABR en insérant les entiers.

```ocaml
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
```

# Compression des ABR

Afin de représenter l’ABR de manière plus compacte en mémoire,  l’idée globale consiste à repérer les sous-arbres (non réduit à une feuille) ayant la même structure arborescente (en oubliant l’étiquetage) puis en remplaçant la deuxième occurrence du sous-arbre via un pointeur vers le premier sous-arbre. 

### Question 2.8

En associant une chaîne de caractères construite sur l'alphabet `{(,)}` à chaque arbre, on peut identifier si deux arbres sont isomorphes.

```ocaml
(*La fonction Ø qui se lit "phi" en Grec*)
let rec phi (a: abr) : string = match a with
  | Vide -> ""
  | Noeud(n) -> "(" ^ (phi n.fg) ^ ")" ^ (phi n.fd);;
```

