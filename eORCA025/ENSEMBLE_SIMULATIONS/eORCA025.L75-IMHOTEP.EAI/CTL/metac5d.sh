#!/bin/bash
if [ $# != 1 ] ; then 
   echo $0   year
   exit
fi

year=$1

CONFIG=eORCA025.L75

for CASE in EAI EGAI ES ; do
   CONFCASE=${CONFIG}-IMHOTEP.${CASE}
   CTL=$PDIR/RUN_${CONFIG}/${CONFCASE}/CTL
   echo $CASE : $CTL
   echo =====
   cd $CTL
     sbatch -J c5d_${CASE}_${year} concat5d.sh $year
     sbatch -J c5d_2_${CASE}_${year} concat5d_2.sh $year
done
