# Projet Ouverture

Fichier `abr.ml`: Toutes les fonctions nécessaires pour `abr` et `abr_comp`  
Fichier `test_fonction.ml`: Tests des fonctions dans `abr.ml`  
Fichier `experimentation.ml`: Étude expérimentale de complexité en écrivant des données des expérimentations dans les fichiers du répertoire */data*. Elle peut durer plusieurs minutes.

Pour compiler, vous avez besoin:
```
ocamlc abr.ml -o abr
ocamlc abr.cmo test_fonction.ml -o test_fonction
ocamlc abr.cmo experimentation.ml -o experimentation
```
puis vous pouvez exécuter `./test_fonction` ou `./experimentation`.

# Tests

```
Test 1.1: Liste L: 1 2 3 4  Liste P:5 

Test 1.2: Liste P: 2 3 5 4 1 

Test 1.7: ( 4 ( 2 ( 1 ε ε ) ( 3 ε ε ) ) ( 8 ( 6 ( 5 ε ε ) ( 7 ε ε ) ) ( 9 ε ε ) ) ) 

Test 2.8: ((())())((())())()

Test 2.9: 4 2 1 3 8 6 5 7 9 

Test 2.10: L'arbre compressé: ( 4 ( 2 ( 1 ε ε ) [ 3 ]->( 1 ε ε ) ) ( 8 [ 6 5 7 ]->( 2 ( 1 ε ε ) ( 3 ε ε ) ) [ 9 ]->( 1 ε ε ) ) ) 

Test 2.11: cherche 7 dans l'arbre : true

Test 3.13: complexité en temps et en mémoire avec n = 100
Temps pour construction abr: 4.5e-05
Temps pour construction abr_comp: 0.012287
Espace de structure abr: 400
Espace de strucuture abr_comp: 369
Temps pour chercher abr: 2e-06
Temps pour chercher abr_comp: 2e-06

```
# Image

Pour espace.png
```
set terminal png
set output 'espace.png'
set grid
set xlabel "nombre de noeuds"
set ylabel "mots stockés en mémoire"
set title "expérimentale de complexité en espace"
plot "abr_espace" using 1:2 title "ABR" with linespoints,"abrcomp_espace" using 1:2 title "ABR compressé" with linespoints

```
Pour temps.png
```
set terminal png
set output 'temps.png'
set grid
set title "expérimentale de complexité en temps"
set xlabel "nombre de noeuds "
set ylabel "sommes de temps pour chercher tous les noeuds"
plot "chercher_abr" using 1:2 title "ABR" with linespoints,"chercher_abrcomp" using 1:2 title "ABR compressé" with linespoints

```
