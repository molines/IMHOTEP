#!/bin/bash


cd $DDIR/ZWK_Xtract_SSS.sh

for f1 in eORCA025.L75-IMHOTEP.GAIa_sosaline*.txt ; do
    f2=$( echo $f1 | sed -e "s/GAIa/GAI/" )
    day0=$( head -1 $f1 | awk '{print $1}' )
    point=$( echo ${f1%.txt} | awk -F_ '{print $NF}' )
    ( cat $f1 | awk '{print $1-day0 " " $2}' day0=$day0 ; echo ; cat $f2 | awk '{print $1-day0 " " $2}' day0=$day0 ) |  graph -T gif -C -L "$point  GAIa (r) GAI (g)" > GAIa-GAI_compare_$point.gif
done
montage tile 2x5 -geometry 400x400 *gif montage.png
