# Projet Ouverture

# Tests

```
Test 1.1: Liste L: 1 2 3 4  Liste P:5 
Test 1.2: Liste P: 2 3 5 4 1 
Test 1.7: ( 4 ( 2 ( 1 ε ε ) ( 3 ε ε ) ) ( 8 ( 6 ( 5 ε ε ) ( 7 ε ε ) ) ( 9 ε ε ) ) ) 
Test 2.8: ((())())((())())()
Test 2.9: 4 2 1 3 8 6 5 7 9 
Test 2.10: L'arbre compressé: ( 4 ( 2 ( 1 ε ε ) [ 3 ]->( 1 ε ε ) ) ( 8 [ 6 5 7 ]->( 2 ( 1 ε ε ) ( 3 ε ε ) ) [ 9 ]->( 1 ε ε ) ) ) 
Test 2.11: cherche 7 dans l'arbre compressé : true
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
