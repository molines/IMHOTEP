#!/bin/bash
CONFIG=eORCA025.L75
CASE=IMHOTEP.EAI

CONFCASE=${CONFIG}-${CASE}

if [ $# = 0 ] ; then
   echo "USAGE : rangexios.sh seg1 seg2  [RM/MV] "
   exit
fi

opt=none
seg1=$1
seg2=$2
opt=$3

if [ $opt = none ] ; then opt="MV" ; fi

if [ $opt  = 'MV' ] ; then
for n in $( seq $seg1 $seg2 ) ; do
  cd $DDIR/${CONFCASE}-XIOS.$n
  ../dcmtk_mvnc2s_mvfast
  cd ../
done
elif [ $opt = 'RM' ] ; then
( for n in $( seq $seg1 $seg2 ) ; do
     rm -rf  $DDIR/${CONFCASE}-XIOS.$n 
done ) &
else
   echo "Option $opt Unknown "
   exit
fi



