#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH --hint=nomultithread
#SBATCH -A cli@cpu
#SBATCH -J JOB_
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=2:00:00
#SBATCH --exclusive

set -x
module load imagemagick/7.0.8-7
CONFIG=eORCA025.L75
CASE=IMHOTEP.GAI
DTADIR=/gpfsstore/rech/cli/rcli002/eORCA025.L75/eORCA025.L75-IMHOTEP.GAI-S/1d/2000-concat

RM=echo
vp="-100 -5 20 70"
#zoom="0 0 4321 3605"
zoom="0 0 1440 1200"
nmax=33

# common to all plots
width=8   # Plot frame in inches
height=6
res=i     # resolution of the coast line c l i h f 
charpal=nrl
proj=noproj     # merc cyl 
bckgrd=shadedrelief   # none etopo shadedrelief bluemarble
bckgrd=etopo          # none etopo shadedrelief bluemarble
depv="deptht"
xstep=15
ystep=15

y1=2000
y2=2000

CONFCASE=${CONFIG}-${CASE}
# S_0 annual clim
figs=./fig_glo_S0
var=vosaline
vmin=30
vmax=40
tick=2
klev=0
mkdir -p $figs

n=0
title=Sgl_0
for y in $(seq $y1 $y2 ) ; do

for f in $DTADIR/${CONFCASE}_y${y}m??_1d_gridT.nc ; do
   ff=$(basename $f )
   g=${ff%.nc} 
   if [ ! -f $figs/$g.png ] ; then
     ( ln -sf $f $ff
     python_plot.py -i $g -v $var  -pc $charpal -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res -klev $klev -depv $depv -tick $tick
      $RM ./$ff  ) &
      
    n=$(( n + 1 ))
    if [ $n = $nmax ] ; then
          wait
          n=0
    fi
   else 
     echo $g.png already done
   fi
done
done
wait

cd $figs
for f in ${CONFCASE}*.png ; do
    convert $f ${f%.png}.gif
done
gifsicle -d100 -l0  ${CONFCASE}*.gif > ${CONFIG}_${title}_MONITOR-$CASE.gif
cd ../

exit

n=0

# in depth salinity
for dep in 150  ; do
title=Sgl_$dep
figs=./fig_glo_S$dep
var=vosaline

case $dep in
(150)
  vmin=30
  vmax=40
  tick=2 ;;
(200)
  vmin=34
  vmax=38
  tick=0.5 ;;
(1000)
  vmin=34
  vmax=36.5
  tick=0.5 ;;
(2000)
  vmin=34.6
  vmax=35.5
  tick=0.1 ;;
(3000)
  vmin=34.6
  vmax=35.05
  tick=0.05 ;;
(4000)
  vmin=34.6
  vmax=35.05
  tick=0.05 ;;
(5000)
  vmin=34.6
  vmax=35.05
  tick=0.05 ;;
esac

mkdir -p $figs

n=0
for y in $(seq $y1 $y2 ) ; do

for f in ../$y/${CONFCASE}_y${y}.1d_gridT.nc ; do
   ff=$(basename $f )
   g=${ff%.nc}
   if [ ! -f $figs/$g.png ] ; then
     ( ln -sf $f $ff
     python_plot.py -i $g -v $var  -pc $charpal -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res -dep $dep -depv $depv -tick $tick
      $RM ./$ff  ) &

    n=$(( n + 1 ))
    if [ $n = $nmax ] ; then
          wait
          n=0
    fi
   else
     echo $g.png already done
   fi
done
done
wait
cd $figs
for f in ${CONFCASE}*.png ; do
    convert $f ${f%.png}.gif
done
gifsicle -d100 -l0  ${CONFCASE}*.gif > ${CONFIG}_${title}_MONITOR-$CASE.gif
cd ../

n=0

done # loop on dep


