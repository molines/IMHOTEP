#!/bin/bash
CONFIG=eORCA025.L75
CASE=IMHOTEP.GAI

CONFCASE=${CONFIG}-${CASE}
MWDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN/
for y in {1980..1982} ; do
   cd $y
   for f in *.nc ; do
     TYP=$(echo ${f%.nc} | awk -F_ '{print $3}' )
     for m in {01..12} ; do
       f1m=$MWDIR/1m/$y/${CONFCASE}_y${y}m${m}.1m_${TYP}.nc
       f1y=${CONFCASE}_y${y}m${m}.1y_${TYP}.nc
       if [ -f $f1m ] ; then
         ln -sf $f1m ./$f1y
       else
         echo $f1m missing
       fi
       # <CONFCASE>_<YEAR>.<freq>_<TYP>.nc
     done
   done
   cd ../
done
