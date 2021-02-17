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
dep=1200
depv="deptht"
xstep=45
ystep=30
tick="-tick 0.02"
clname='Salinity '
lorca="-orca"
title1="WOA18 5564 - CLIM ${dep}m "



mkdir -p $figs

   ff=eORCA025.L75_5564-CLIM_WOA18_1m_vosaline.nc
for m in {1..12} ; do
   mm=$( printf "%02d" $m )
   case $m in
   ( 1) month=JAN ;;
   ( 2) month=FEB ;;
   ( 3) month=MAR ;;
   ( 4) month=APR ;;
   ( 5) month=MAY ;;
   ( 6) month=JUN ;;
   ( 7) month=JUL ;;
   ( 8) month=AUG ;;
   ( 9) month=SEP ;;
   (10) month=OCT ;;
   (11) month=NOV ;;
   (12) month=DEC ;;
   esac

   title2="Relative Salinity Anomaly  $month "

   g=${ff%.nc} 
   if [ ! -f $figs/$g.png ] ; then
#      ln -sf $f $ff
     time_record=$(( m - 1 ))
     python_plot.py -i $g -v $var  -p $pal  -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res -dep $dep -depv $depv $tick -t $time_record -nt 1 \
            --long_name "$clname"  $lorca -tit1 "$title1" -tit2 "$title2"
      mv $figs/$g.png $figs/${g}_${mm}.png
      
   else 
     echo $g.png already done
   fi
done
