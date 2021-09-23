#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A cli@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_
#SBATCH -e zjobacc.e%j
#SBATCH -o zjobacc.o%j
#SBATCH --time=2:30:00
#SBATCH --exclusive


#set -x
CONFIG=eORCA025.L75
CASE=IMHOTEP01

freq=1m
CONFCASE=${CONFIG}-${CASE}

vp="-180 -90 180 -50"
#zoom="3 1 4322 2000"
zoom="3 1 1442 667"
figs=./fig_acc_month
#pal=RdBu_r
pal=gist_ncar_r
proj=spaeqd

xstep=30
ystep=20

y1=1958
y2=1968

mkdir -p $figs


for y in $(seq $y1 $y2 ) ; do

for f in ../$y/${CONFCASE}_y${y}m??.${freq}_ICBT.nc ; do
   ff=$(basename $f )
   ln -sf $f $ff
   g=${ff%.nc} 
   if [ ! -f $figs/$g.png ] ; then
(   ./south_python_plot.py -i $g -v berg_melt -p $pal -proj $proj -xstep $xstep -ystep $ystep \
        -wij $zoom -wlonlat $vp  -d $figs > log  ; rm ./$ff) &
   n=$(( n + 1 ))
   else 
     echo $g.png already done
   fi
   if [ $n = 30 ] ; then
      wait
      n=0
   fi
done
done
wait
