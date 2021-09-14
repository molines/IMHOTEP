#!/bin/bash

CONFIG=eORCA025.L75
CASE1=IMHOTEP.S
CASE2=IMHOTEP.GAI

region=ATLTROP
var=sosaline

SRC1=$WORK/${CONFIG}/${CONFIG}-${CASE1}-PLOT/$var/$region
SRC2=$WORK/${CONFIG}/${CONFIG}-${CASE2}-PLOT/$var/$region

cd $DDIR
mkdir -p zmontage_${CONFIG}_${CASE1}-${CASE2}_${var}_$region
cd zmontage_${CONFIG}_${CASE1}-${CASE2}_${var}_$region

for f1 in $SRC1/${CONFIG}-${CASE1}_y*_${var}_${region}_*.png ; do
    g=$(basename $f1 )
    tmp=$( echo $g | sed -e "s/$CASE1/$CASE2/" )
    f2=$SRC2/$tmp
    
    MTG=$(echo $g | sed -e "s/$CASE1/S-GAI/" )
  
    if [ ! -f $MTG ] ; then 
      montage -tile 2x1 -geometry 900x900 $f1 $f2 $MTG
      echo $MTG done
    else
      echo $MTG exists already !
    fi
done


