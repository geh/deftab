#!/bin/bash

PROVER="deftab"
FORMULAS="."

echo "# Intuitionistic theorems (should all be consequence). #"
for file in `ls ${FORMULAS}/vandalen*`;
do
   echo $file;
   ${PROVER} -f $file $1 $2 $3 $4 | grep consequence;
done
for file in `ls ${FORMULAS}/wiki*`;
do
   echo $file;
   ${PROVER} -f $file $1 $2 $3 $4 | grep consequence;
done

for file in `ls ${FORMULAS}/theorem*`;
do
   echo $file;
   ${PROVER} -f $file $1 $2 $3 $4 | grep consequence;
done


echo "# Hydrid SAT formulas as consequence (should not be csq unless valid). #"

for file in `ls ${FORMULAS}/hybrid_sat/*`;
do
    echo $file;
    ${PROVER} -f $file $1 $2 $3 $4 | grep consequence;
done

for file in `ls ${FORMULAS}/hybrid_unsat/*`;
do
    echo $file;
    ${PROVER} -f $file $1 $2 $3 $4 | grep consequence;
done

