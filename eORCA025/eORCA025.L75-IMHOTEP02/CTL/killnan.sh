#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=21
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A bcn@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_NAN
#SBATCH -e zjobnan.e%j
#SBATCH -o zjobnan.o%j
#SBATCH --time=2:30:00
#SBATCH --exclusive

CONFIG=eORCA025.L75
CASE=IMHOTEP02

CONFCASE=${CONFIG}-${CASE}

for typ in PRODU PRODV gridU gridV gridT ; do
   n=0
  for y in {1978..2019} ; do
     cd $y
     cdfnan -l ${CONFCASE}_y${y}.1y_$typ.nc &
     n=$(( n + 1 ))
     cd ../
     if [ $n = 21 ] ; then
       wait
       n=0
     fi
  done
wait
done
wait
