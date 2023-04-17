#!/bin/sh

set -x
cd ../src/

rm -rf  htab *.o *.hi HTab/*.o HTab/*.hi *.tix ../html
ghc --make -fhpc -fglasgow-exts htab.hs

HTAB=./htab
DATA=../examples/sat

 for i in $DATA/*; do
   $HTAB --showformula                   -f $i
   $HTAB -v --stats=10                   -f $i
   $HTAB -t 2                            -f $i
   $HTAB -t 2 --lazybranching=False      -f $i
   $HTAB -t 2 --eager                    -f $i
   $HTAB -t 2 --unit-prop                -f $i
   $HTAB -t 2 --no-unit-prop             -f $i
   $HTAB -t 2 --alltransitive            -f $i
   $HTAB -t 2 --allreflexive             -f $i
   $HTAB -t 2 --allsymmetric             -f $i
   $HTAB -t 2 --allfunctional            -f $i
   $HTAB -t 2 --allinjective             -f $i
   $HTAB -t 2 -m mod                     -f $i
 done 

DATA=../examples/unsat

 for i in $DATA/*; do
   $HTAB --showformula                   -f $i
   $HTAB -v --stats=10                   -f $i
   $HTAB -t 2                            -f $i
   $HTAB -t 2 --lazybranching=False      -f $i
   $HTAB -t 2 --eager                    -f $i
   $HTAB -t 2 --unit-prop                -f $i
   $HTAB -t 2 --no-unit-prop             -f $i
   $HTAB -t 2 --alltransitive            -f $i
   $HTAB -t 2 --allreflexive             -f $i
   $HTAB -t 2 --allsymmetric             -f $i
   $HTAB -t 2 --allfunctional            -f $i
   $HTAB -t 2 --allinjective             -f $i
   $HTAB -t 2 -m mod                     -f $i
 done 

DATA=../examples/sat_no_mod

 for i in $DATA/*; do
   $HTAB  --showformula                  -f $i
   $HTAB -v --stats=10                   -f $i
   $HTAB -t 2                            -f $i
   $HTAB -t 2 --lazybranching=False      -f $i
   $HTAB -t 2 --eager                    -f $i
   $HTAB -t 2 --unit-prop                -f $i
   $HTAB -t 2 --no-unit-prop             -f $i
   $HTAB -t 2 -m mod                     -f $i
 done 

$HTAB --help
hpc markup htab --destdir=../html

rm -rf htab htab.tix mod *.o *.hi HTab/*.o HTab/*.hi out*
