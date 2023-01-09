#!/bin/bash
if [ $# != 2 ] ; then 
   echo $0   seg1 seg2 
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
   sbatch jobobs_3 ${CONFCASE}  $seg1 $seg2 
   sbatch jobtrj_3 ${CONFCASE}  $seg1 $seg2 
done
