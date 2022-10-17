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

freq=1d

ylst=( {1975..1979} )

for m in {001..010} ; do
   for y in ${ylst[@]} ; do
     CONFCASE=${CONFIG}-${CASE}.$m
     cd $DDIR/$CONFIG/${CONFCASE}-S/$freq
     if [ -d $SDIR/$CONFIG/${CONFCASE}-S/$freq/${y}-concat ] ; then
       rm -rf ${y}-concat
       echo year ${y}-concat  erased for member $m
     else
       echo  ${y}-concat  NOT erased for member $m : not in store !!!
     fi
   done
done
