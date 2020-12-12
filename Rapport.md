

# Présentation

Dans ce projet, nous cherchons une solution de manipuler un modèle de structure de données arborescence, les Arbres de Binaires de Recherche puis d'en construire une structure compressée suivant une procédure bien particulière.

[TOC]



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

### Question 1.7

En prenant ces deux cas, nous avons défini un type `abr` pour présenter un arbre binaire.

```ocaml
type abr = 
  | Noeud of {etq: int; fg: abr; fd: abr}
  | Vide;;
```

`etq` présente l'étiquette sur le nœud, `fg` et `fd` représentent le fils gauche et le fils droit respectivement du nœud.

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

![Figure 1](https://i.loli.net/2020/12/01/galW8NboQXV2Z61.png)

Une version imprimée du figure au dessus est la suivante: 

```
( 4 ( 2 ( 1 ε ε ) ( 3 ε ε ) ) ( 8 ( 6 ( 5 ε ε ) ( 7 ε ε ) ) ( 9 ε ε ) ) ) 
```

# Compression des ABR

Afin de représenter l’ABR de manière plus compacte en mémoire,  l’idée globale consiste à repérer les sous-arbres (non réduit à une feuille) ayant la même structure arborescente (en oubliant l’étiquetage) puis en remplaçant la deuxième occurrence du sous-arbre via un pointeur vers le premier sous-arbre. 

### Question 2.8

En associant une chaîne de caractères construite sur l'alphabet `{(,)}` à chaque arbre, nous pouvons identifier si deux arbres sont isomorphes.

```ocaml
(*La fonction Ø qui se lit "phi" en Grec*)
let rec phi (a: abr) : string = match a with
  | Vide -> ""
  | Noeud(n) -> "(" ^ (phi n.fg) ^ ")" ^ (phi n.fd);;
```

### Question 2.9

Nous calculons un tableau contenant les étiquettes de l’arbre rangées en ordre préfixe.

```ocaml
let rec prefixe (a: abr) : int list = match a with
  | Vide -> []
  | Noeud(n) -> (n.etq)::(prefixe n.fg)@(prefixe n.fd);;
```

### Question 2.10

Pour compresser un ABR, il est nécessaire de définir une nouvelle structure de données.

```ocaml
type abr_comp = 
  | VideComp
  | NoeudComp of {etq: int; fg: abr_comp; fd: abr_comp; }
  | Pointeur of {etqs: int array; mutable point: abr_comp};;
```

Le record `Pointeur` représente un sous-arbre par un pointeur qui stocke les étiquettes contenues dans ce sous-arbre dans le tableau `etqs`.  L'attribut `point` représente un pointer implicite vers un sous-arbre ayant le même structure.

Pour ce nouveau type, nous devons réécrire les méthodes `phi` et  `prefixe`(voir dans le fichier abr.ml). La construction d'un ABR compressé commence par initialiser un ABR compressé sans `Pointeur` par copie d'un ABR non compressé.

```ocaml
let rec init (a: abr) : abr_comp = match a with
  | Vide -> VideComp
  | Noeud(x) -> NoeudComp {etq = x.etq; fg = (init x.fg); fd = (init x.fd)};;
```

Puis nous cherchons tous les sous-arbres dans cet ABR compressé. Dans le figure au dessus, nous avons 9 sous-arbres. En ordre préfixe ils sont présentés comme les suivants: 

- 1
- 3
- 2-1-3
- 5
- 7
- 6-5-7
- 9
- 8-6-5-7-9
- 4-2-1-3-8-6-5-7-9

```ocaml
let rec arbres (a: abr_comp) : (abr_comp ref list) = match a with
  | VideComp -> []
  | NoeudComp(n) -> (arbres n.fg)@(arbres n.fd)@[(ref a)]
  | Pointeur(n) -> [];;
```

Pour remplacer un nœud par un `Pointeur`, nous devons trouver un ABR compressé qui a une même structure comme ce nœud dans la liste renvoyée par la fonction `arbres`.

```ocaml
let rec find (a: abr_comp) (l: abr_comp list) : abr_comp =
  match l with
  | [] -> VideComp
  | x::xs -> if (egal_structure a x)  then x else (find a xs)
```

La fonction `egal_structure` permet de savoir l'égalité de structures de deux ABR compressés.

Ayant fait toute la préparation, on peut commencer à remplacer des nœuds par `Pointeur` en parcourant l'ABR compressé initialisé par `init`.

```ocaml
let rec construction_comp (a: abr) : abr_comp = match a with
  | Vide -> VideComp
  | Noeud(n) ->
    let ab = (init a) in
    let l = (arbres ab) in
    let rec replace (a: abr_comp) = 
    let e = (find a l) in 
      if (identique a e) = true
      then match a with
        | VideComp -> VideComp
        | NoeudComp(x) -> NoeudComp {etq = x.etq; fg = (replace x.fg ); fd = (replace x.fd )}
        | Pointeur(x) -> Pointeur {etqs = x.etqs; point = x.point}
      else match a with
        | VideComp -> VideComp
        | _ -> Pointeur {etqs = (prefixe_comp a); point = e}
    in (replace ab );;
```

On vérifie que l'ABR compressé  `a` est identique avec un autre ABR compressé `e` trouvé par `find` dans la liste `l` renvoyée par `arbres` (si les résultats de préfixe sont identiques). S'ils ne sont pas identiques, alors `a` peut être remplacé par un pointer vers `e`. Sinon, on laisse ce nœud inchangé et continue à remplacer ses sous arbres récursivement.

![figure 2](https://i.loli.net/2020/12/01/DG1OdKYkynep7uv.png)

Une version imprimée du figure au dessus est: 

```
( 4 ( 2 ( 1 ε ε ) [ 3 ]->( 1 ε ε ) ) ( 8 [ 6 5 7 ]->( 2 ( 1 ε ε ) ( 3 ε ε ) ) [ 9 ]->( 1 ε ε ) ) ) 
```

### Question 2.11

Soit l'arbre en cours de visiter `a`. Si on tombe dans la cherche d'un élément `e` dans un tableau d'étiquettes, sachant que le premier élément `x` présente la racine de `a`. En  comparant `e` et `x`, on peut savoir l'élément `e` est dans le fils droit ou gauche de `a` puis nous utilisons une indice `i`  du tableau `etqs` pour nous aider à sauter dans **la partie correspondante**  pour visiter le tableau d'étiquettes plus efficacement.

L'indice `i` est initialisée à 0, elle dépend la position de l'élément x: 

- Si dans le fils droit, `i = i + 1`
- Si dans le fils gauche, `i = i + 1 + taille(filsGauche a)`

```ocaml
let rec chercher_comp (a: abr_comp) (e: int) : bool = match a with
  | VideComp -> false
  | NoeudComp(n) -> 
    if (e < n.etq)
    then (chercher_comp n.fg e)
    else if (e > n.etq) 
    then (chercher_comp n.fd e)
    else true
  | Pointeur(n) -> 
    let i = (ref 0) and a = (ref n.point) and result = (ref false) in
    begin
      while !i < (Array.length n.etqs) && (!result = false) do
        if (e < n.etqs.(!i)) then
          begin
            i := !i + 1; 
            a := (filsGauche !a);
          end
        else if (e > n.etqs.(!i)) then
          begin
            i := !i + 1 + (taille_comp (filsGauche !a));
            a := (filsDroit !a);
          end
        else result := true
      done;
      !result;
    end;;    
```



### Question 2.13

 La complexité en moyenne de la recherche dans un ABR compressé dépend de la procédure de visiter le tableau d'étiquettes. La complexité de notre algorithme de la fonction *chercher* est en $O(n)$.

# Expérimentations

### Question 3.13

Nous proposons la fonction suivante pour calculer le temps pris par l'exécution d'un algorithme.

```ocaml
let time f x: float =
  let t = Sys.time() in
  let fx = f x in
  fx;
  (Sys.time() -. t);;
```

L'argument `f` est la fonction à tester alors `x` est l'argument de `f`. 

Pour calculer l’espace mémoire occupé par une structure, **nous utilisons la fonction `sizeof` proposée dans l'énoncé.** 

On peur déduire qu'un ABR non compressé ayant seulement un nœud interne (racine) a la taille de **4 mots** qui sont 

- 3 valeurs: étiquette, fils gauche, fils droit
- son en-tête

Pour un ABR compressé, le record `Pointeur` qui sont composé par un tableau et un pointer vers un ABR non compressé. Soit le nombre d'étiquettes dans le tableau `x`, on peut déduire qu'un record `Pointer` peut avoir **`1 + x + 4 ` mots** qui sont

- en-tête de tableau: 1 mot
- `x` éléments: x mots
- pointer vers un ABR non compressé: 4 mots

### Question 3.14

![temps.png](https://i.loli.net/2020/12/13/9xmP1ZBNiy6tbQH.png)

L'axe des ordonnées est la somme de temps pour chercher tous les nœuds dans un ABR. S'il y a `n` entiers dans un ABR `a`, alors la valeur est `(time_chercher chercher a 1) +(time_chercher chercher a 2) + ... + (time_chercher chercher a n)`.

La fonction *chercher*  dans un *ABR compressé* doit manipuler le tableau donc nous observons que *ABR compressé* prends plus de temps sur le même nombre de nœuds que *ABR* mais les deux fonctions *chercher* dans *ABR* et dans *ABR compressé* ont la même complexité. 

### Question 3.15

Pour comparer la comparaison d'ABR de différentes tailles, nous introduisons 2 cas: ABR de 50 - 500 nœuds et ABR de 500 - 5000 nœuds.

![espace_petit.png](https://i.loli.net/2020/12/13/BVIlk3ezZHqNGE7.png)

D'après le graphe, nous pouvons remarquer que l'ABR compressé prends plus d'espace de mémoire que l'ABR non compressé sur une petite taille. Mais au fur à l'augmentation de taille, l'ABR compressé a une bonne optimisation.

![espace.png](https://i.loli.net/2020/12/13/mascnk2TG1NU8zg.png)

Dans le cas où la taille d'ABR est assez grand, l'ABR compressé peut contenir plus de `Pointeur` qui peuvent épargner l'espace de mémoire. On peut conclure que notre algorithme a une meilleur performance pour compresser un ABR de grande taille.