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
   for m in {001..010} ; do
     sbatch -J 5d_${CASE}_${year}_$m job_5d.sh $year $m
     sbatch -J ssh5d_${CASE}_${year}_$m job_ssh5d.sh $year $m
   done
done
