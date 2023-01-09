#!/bin/bash

if [ $# = 0 ] ; then
   echo " USAGE : rmgridTUVW.sh go "
   echo "    erase all grid TUVW files from 1d directory"
   echo "   NO WAY TO GO BACK !!! "
   exit
fi

CONFIG=eORCA025.L75
CASE=IMHOTEP.ES

freq=1d

ylst=( {2018..2018} )

for m in {001..010} ; do
   for y in ${ylst[@]} ; do
     CONFCASE=${CONFIG}-${CASE}.$m
     cd $DDIR/$CONFIG/${CONFCASE}-S/$freq/${y}/
     echo Number of files to erase for member $m year $y : $(ls -l ${CONFCASE}*grid[TUVW].nc | wc -l )
     rm ${CONFCASE}*grid[TUVW].nc
   done
done
