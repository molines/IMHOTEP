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
#SBATCH --time=2:30:00
#SBATCH --exclusive

#set -x
dep=1200
PERIOD1=5564
PERIOD2=81B0
title1="WOA18 $PERIOD1 - $PERIOD2 $dep m "
ff=eORCA025.L75_${PERIOD1}-${PERIOD2}_WOA18_1y_vosaline.nc

vp=" -286 -80 72 85"
zoom="2 2 1440 1200"
figs=./fig_woa18_sal1200
var=vosaline
pal=RdBu_r
charpal=nrl
proj=cyl #noproj     # merc cyl 
bckgrd=none   # none etopo shadedrelief bluemarble
vmax=+0.14
vmin=-0.14
width=9   # Plot frame in inches
height=5
res=i     # resolution of the coast line c l i h f 
klev=48
depv="deptht"
xstep=45
ystep=30
tick="-tick 0.02"
clname='Salinity '
lorca="-orca"
title2="Relative Salinity Anomaly Annual Mean"



mkdir -p $figs

   g=${ff%.nc} 
   if [ ! -f $figs/$g.png ] ; then
#      ln -sf $f $ff
     python_plot.py -i $g -v $var  -p $pal  -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res -dep $dep  -depv $depv $tick -t 0 -nt 1 \
            --long_name "$clname"  $lorca -tit1 "$title1" -tit2 "$title2"
      
   else 
     echo $g.png already done
   fi
