#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A bcn@cpu
#SBATCH --hint=nomultithread
#SBATCH -J MONTAGE
#SBATCH -e zmtg.e%j
#SBATCH -o zmtg.o%j
#SBATCH --time=0:30:00
#SBATCH --exclusive


CONFIG=eORCA025.L75
CASE1=IMHOTEP.GAI
CASE2=IMHOTEP.GAIa

region=ATLIND
var=sosaline

nmax=40

SRC1=$WORK/${CONFIG}/${CONFIG}-${CASE1}-PLOT/$var/$region
SRC2=$WORK/${CONFIG}/${CONFIG}-${CASE2}-PLOT/$var/$region

cd $DDIR
mkdir -p zmontage_${CONFIG}_${CASE1}-${CASE2}_${var}_$region
cd zmontage_${CONFIG}_${CASE1}-${CASE2}_${var}_$region
n=0
for f1 in $SRC1/${CONFIG}-${CASE1}_y*_${var}_${region}_*.png ; do
    ( g=$(basename $f1 )
    tmp=$( echo $g | sed -e "s/$CASE1/$CASE2/" )
    f2=$SRC2/$tmp
    
    MTG=$(echo $g | sed -e "s/$CASE1/S-GAI/" )
  
    if [ ! -f $MTG ] ; then 
      montage -tile 2x1 -geometry 900x900 $f1 $f2 $MTG
      echo $MTG done
    else
      echo $MTG exists already !
    fi ) &
    n=$(( n + 1 ))
    if [ $n = $nmax ] ; then
      wait
      n=0
    fi
done
wait


