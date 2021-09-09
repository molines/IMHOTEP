#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH --hint=nomultithread
#SBATCH -A bcn@cpu
#SBATCH -J JOB_
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=0:30:00
#SBATCH --exclusive

#set -x

usage() {
   echo
   echo "USAGE : $(basename $0 ) -c CONFCASE -y YEAR | -p period "
   echo
   echo "   PURPOSE:"
   echo "     Plot EKE field  for year YEAR and CONFCASE"
   echo
   echo "  OPTIONS:"
   echo "     -c CONFCASE : pass CONFCASE to work with "
   echo "     -y YEAR : Year to plot "
   echo "     -p period : pass the period to plot (e.g. 2009-2018"
   echo
   exit 0
        }

if [ $# = 0 ] ; then usage ; fi

freq=1y
while getopts :hc:y:p:  opt ; do
   case $opt in
     (h) usage ;;
     (c) CONFCASE=${OPTARG} ; CONFIG=${CONFCASE%-*} ; CASE=${CONFCASE#*-} ;;
     (y) y1=${OPTARG} ; y2=$y1 ;;
     (p) period=${OPTARG} ; y1=${period%-*} ; y2=${period#*-} ;;
     (\?) usage ;;
   esac
done

MWDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN/$freq/
FIGDIR=$WORK/${CONFIG}/${CONFCASE}-PLOT/EKE/PACIF/LOG/


vp="-100 -70 120 70"
vp="-240 -70 -70  70"
zoom="80 112 915 1114"
figs=$FIGDIR
var=voeke
pal=YlGnBu_r
charpal=nrl
proj=merc     # merc cyl 
#bckgrd=shadedrelief   # none etopo shadedrelief bluemarble
bckgrd=none   # none etopo shadedrelief bluemarble
vmax=-9999
vmin=-9999
clrlim='EKE.clrlim.$$'
width=7   # Plot frame in inches
height=6
res=i     # resolution of the coast line c l i h f 
klev=-1
dep=10
depv="deptht"


xstep=30
ystep=30

cat << eof > $clrlim
1
10
100
1000
5000
eof

mkdir -p $figs

nmax=39
n=0
for y in $(seq $y1 $y2 ) ; do
   MWDIRY=$MWDIR/$y

for f in $MWDIRY/${CONFCASE}_y${y}.${freq}_EKE.nc ; do
   ff=$(basename $f )
   g=${ff%.nc} 
   cf_out=${CONFCASE}_y${y}.${freq}_EKElog
   if [ ! -f $figs/$g.png ] ; then
     ( ln -sf $f $ff
      python_plot.py -i $g -v $var  -p $pal -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res -klev $klev -depv $depv -pc $charpal -dep $dep -clrlim $clrlim -log -o $cf_out  -orca
      rm ./$ff   ) &
      
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
rm ./$clrlim
