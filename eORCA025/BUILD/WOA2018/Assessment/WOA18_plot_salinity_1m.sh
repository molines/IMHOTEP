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
#PERIOD=5564  # CLIM
PERIOD=81B0  # CLIM 81B0
#PERIOD=CLIM  # CLIM
###
vp=" -286 -80 72 85"
zoom="2 2 1440 1200"
figs=./fig_woa18_sal
var=vosaline
pal=YlGnBu_r
charpal=nrl
proj=cyl #noproj     # merc cyl 
bckgrd=none   # none etopo shadedrelief bluemarble
vmax=39
vmin=26
width=9   # Plot frame in inches
height=5
res=i     # resolution of the coast line c l i h f 
klev=0
depv="deptht"
xstep=45
ystep=30
tick="-tick 1"
clname='Salinity '
lorca="-orca"
case $PERIOD in
(5564)  title1="WOA18 1955-1964" ;;
(81B0)  title1="WOA18 1981-2010" ;;
(CLIM)  title1="WOA18 1955-2017" ;;
esac



mkdir -p $figs

   ff=eORCA025.L75_${PERIOD}_WOA18_1m_vosaline.nc
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

   title2="Relative Salinity  $month "

   g=${ff%.nc} 
   if [ ! -f $figs/$g.png ] ; then
#      ln -sf $f $ff
     time_record=$(( m - 1 ))
     python_plot.py -i $g -v $var  -p $pal -pc $charpal -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res -klev $klev -depv $depv $tick -t $time_record -nt 1 \
            --long_name "$clname"  $lorca -tit1 "$title1" -tit2 "$title2"
      mv $figs/$g.png $figs/${g}_${mm}.png
      
   else 
     echo $g.png already done
   fi
done
