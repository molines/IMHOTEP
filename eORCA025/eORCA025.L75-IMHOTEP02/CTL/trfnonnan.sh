#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=21
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A bcn@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_ST
#SBATCH -e zjobst.e%j
#SBATCH -o zjobst.o%j
#SBATCH --time=2:30:00
#SBATCH --exclusive

CONFIG=eORCA025.L75
CASE=IMHOTEP02

CONFCASE=${CONFIG}-${CASE}

SYDIR=$SDIR/${CONFIG}/${CONFCASE}-S/1y
WYDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN/1y

for typ in PRODU PRODV gridU gridV gridT ; do
   n=0
  for y in {1978..2019} ; do
     cd $y
     f=${CONFCASE}_y${y}.1y_$typ.nc
       dd bs=600M if=$f of=$SYDIR/$y/$f &
       dd bs=600M if=$f of=$WYDIR/$y/$f &
     n=$(( n + 2 ))
     cd ../
     if [ $n -gt 19  ] ; then
       wait
       n=0
     fi
  done
wait
done
wait
