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

set -x

vp="-100 -70 20 70"
zoom="2112 336 3840 3340"
figs=./fig_natl3d_eke_log
var=voeke
pal=YlGnBu_r
charpal=nrl
proj=merc     # merc cyl 
bckgrd=shadedrelief   # none etopo shadedrelief bluemarble
vmin=-9999
vmax=-9999
clrlim='EKE.clrlim'
width=7   # Plot frame in inches
height=6
res=i     # resolution of the coast line c l i h f 
klev=-1
dep=10
depv="deptht"


xstep=30
ystep=30

y1=1980
y2=1981

cat << eof > $clrlim
1
10
100
1000
5000
eof

mkdir -p $figs

nmax=36
n=0
for y in $(seq $y1 $y2 ) ; do

for f in ../$y/eORCA12.L75-GJM2020_y${y}.1d_EKE.nc ; do
   ff=$(basename $f )
   g=${ff%.nc} 
   if [ ! -f $figs/$g.png ] ; then
     ( ln -sf $f $ff
      python_plot.py -i $g -v $var  -p $pal -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res -klev $klev -depv $depv -dep $dep -pc $charpal -clrlim $clrlim -log
      rm ./$ff  ) &
      
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
