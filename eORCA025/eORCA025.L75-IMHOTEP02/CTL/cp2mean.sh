#!/bin/bash

CONFIG=eORCA025.L75
CASE=IMHOTEP02

y1=1958
y2=2019

CONFCASE=${CONFIG}-${CASE}

MWDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN
MSDIR=$DDIR/${CONFIG}/${CONFCASE}-S


for freq in  1y; do
  cd $MSDIR/$freq
  for y in $( seq $y1 $y2 ) ; do
     mkdir -p $MWDIR/$freq/$y
     cd $y
        for f in ${CONFCASE}_y${y}*.nc ; do
           echo -n "transfering  $f ..."
           dd bs=600M if=$f of=$MWDIR/$freq/$y/$f
           echo "... done."
        done 
     cd ../
  done
done
