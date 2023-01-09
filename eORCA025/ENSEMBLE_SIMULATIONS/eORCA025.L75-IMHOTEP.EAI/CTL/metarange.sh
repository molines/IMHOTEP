#!/bin/bash
if [ $# != 3 ] ; then 
   echo $0   seg1 seg2  MV/RM
   exit
fi

seg1=$1
seg2=$2

CONFIG=eORCA025.L75

for CASE in EAI EGAI ES ; do
   CONFCASE=${CONFIG}-IMHOTEP.${CASE}
   CTL=$PDIR/RUN_${CONFIG}/${CONFCASE}/CTL
   echo $CASE
   echo =====
   cd $CTL
   ./rangexios.sh $seg1 $seg2 $3
done
