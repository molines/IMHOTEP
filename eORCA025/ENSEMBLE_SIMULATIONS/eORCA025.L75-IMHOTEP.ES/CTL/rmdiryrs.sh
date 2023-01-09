#!/bin/bash

if [ $# = 0 ] ; then
   echo " USAGE : rmdiryrs go "
   echo "    erase all year directory  from 1d directory"
   echo "    Assume that 5d average are safely kept and that concatenation is done"
   echo "   NO WAY TO GO BACK !!! "
   exit
fi

CONFIG=eORCA025.L75
CASE=IMHOTEP.ES

freq=5d

ylst=( {2018..2018} )

for m in {001..010} ; do
   for y in ${ylst[@]} ; do
     CONFCASE=${CONFIG}-${CASE}.$m
     cd $DDIR/$CONFIG/${CONFCASE}-S/$freq
     rm -rf ${y}
     echo year $y erased for member $m
   done
done
