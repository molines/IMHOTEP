#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A cli@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_
#SBATCH -e zjobgro.e%j
#SBATCH -o zjobgro.o%j
#SBATCH --time=2:30:00
#SBATCH --exclusive

#set -x
CONFIG=eORCA025.L75
CASE=IMHOTEP01

freq=1m
CONFCASE=${CONFIG}-${CASE}


vp="-75 30 10 85"
zoom="3 667 1442 1200"
pal=gist_ncar_r
#proj=npaeqd
proj=stere

xstep=10
ystep=10

y1=1958
y2=1968

var=vfxsnw
figs=./fig_${var}_monthly_gro_stere
mkdir -p $figs

n=0
for y in $(seq $y1 $y2 ) ; do

for f in ../$y/${CONFCASE}_y${y}m??.${freq}_icemod.nc ; do
   ff=$(basename $f )
   ln -sf $f $ff
   g=${ff%.nc} 
   if [ ! -f $figs/$g.png ] ; then
   ( ./north_stere_python_plot.py -i $g -v $var -p $pal -proj $proj -xstep $xstep -ystep $ystep \
        -wij $zoom -wlonlat $vp  -d $figs > log ; rm ./$ff )&
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
