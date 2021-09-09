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
FIGDIR=$WORK/${CONFIG}/${CONFCASE}-PLOT/TEMPERATURE/ATLIND


vp="-100 -70 120 70"
#zoom="2112 336 3840 3340"
#zoom="704 112 1280 1114"
zoom="704 112 231 1114"
figs=$FIGDIR
var=votemper
pal=YlGnBu_r
charpal=nrl
proj=merc     # merc cyl 
bckgrd=shadedrelief   # none etopo shadedrelief bluemarble
bckgrd=none   # none etopo shadedrelief bluemarble
vmin=-2
vmax=30
tick=4
width=7   # Plot frame in inches
height=6
res=i     # resolution of the coast line c l i h f 
klev=0
dep=10
depv="deptht"


xstep=30
ystep=30


mkdir -p $figs

nmax=39
n=0
for y in $(seq $y1 $y2 ) ; do
   MWDIRY=$MWDIR/$y

for f in $MWDIRY/${CONFCASE}_y${y}.${freq}_gridT.nc ; do
   ff=$(basename $f )
   g=${ff%.nc} 
   cf_out=${CONFCASE}_y${y}.${freq}_CT

   if [ ! -f $figs/$g.png ] ; then
     ( ln -sf $f $ff
      python_plot.py -i $g -v $var  -p $pal -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res -klev $klev  -pc $charpal  -tick $tick -o $cf_out
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
