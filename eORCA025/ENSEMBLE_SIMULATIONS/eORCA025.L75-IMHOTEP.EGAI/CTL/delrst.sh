#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH -A cli@cpu
#SBATCH -J JOB_delrst
#SBATCH -e zdelrst.e%j
#SBATCH -o zselrst.o%j
#SBATCH --time=1:00:00

cd $DDIR
for n in {52..58} ; do
   rm -rf  eORCA025.L75-IMHOTEP.ES-RST.$n
   echo eORCA025.L75-IMHOTEP.ES-RST.$n deleted for ever ! 
done
