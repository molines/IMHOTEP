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
   echo "USAGE : $(basename $0 ) -c CONFCASE -y YEAR | -p period -r REGION"
   echo
   echo "   PURPOSE:"
   echo "     Plot EKE field  for year YEAR and CONFCASE"
   echo
   echo "  OPTIONS:"
   echo "     -c CONFCASE : pass CONFCASE to work with "
   echo "     -y YEAR : Year to plot "
   echo "     -p period : pass the period to plot (e.g. 2009-2018"
   echo "     -r REGION : gives predifined region name ATLIND PACIF ATLTROP "
   echo
   exit 0
        }

if [ $# = 0 ] ; then usage ; fi

conda activate BASEMAP

freq=1y
region=none
while getopts :hc:y:p:r:  opt ; do
   case $opt in
     (h) usage ;;
     (c) CONFCASE=${OPTARG} ; CONFIG=${CONFCASE%-*} ; CASE=${CONFCASE#*-} ;;
     (y) y1=${OPTARG} ; y2=$y1 ;;
     (p) period=${OPTARG} ; y1=${period%-*} ; y2=${period#*-} ;;
     (r) region=${OPTARG} ;;
     (\?) usage ;;
   esac
done

MWDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN/$freq/


if [ $region = ATLTROP ] ; then
  # ATLTROP
  vp="-100 -15 15 40"
  zoom="740 580 1230 910"
  orcaopt=''
  xstep=10
  ystep=10
elif [ $region = NINDIAN ] ; then
  # NINDIAN
  vp="44 2 106  28"
  zoom="1300 650 160 820"
  orcaopt=''
  xstep=10
  ystep=10
elif [ $region = PACIF ] ; then
  # PACIF
  vp="-240 -70 -70  70"
  zoom="80 112 915 1114"
  orcaopt='-orca'
  xstep=30
  ystep=30
elif [ $region = ATLIND ] ; then
  # ATLIND
  vp="-100 -70 120 70"
  zoom="704 112 231 1114"
  orcaopt=''
  xstep=30
  ystep=30
else
  echo $region not defined yet !
  exit
fi

var=vosaline
unit='g/kg'
FIGDIR=$WORK/${CONFIG}/${CONFCASE}-PLOT/$var/$region

figs=$FIGDIR
pal=YlGnBu_r
charpal=nrl
proj=merc     # merc cyl 
bckgrd=shadedrelief   # none etopo shadedrelief bluemarble
bckgrd=none   # none etopo shadedrelief bluemarble
vmin=24
vmax=38
width=7   # Plot frame in inches
height=6
res=i     # resolution of the coast line c l i h f 
klev=0
dep=10
depv="deptht"


tick=2


mkdir -p $figs

nmax=39
n=0
for y in $(seq $y1 $y2 ) ; do
   MWDIRY=$MWDIR/$y

for f in $MWDIRY/${CONFCASE}_y${y}.${freq}_gridT.nc ; do
   ff=$(basename $f )
   g=${ff%.nc} 
   cf_out=${CONFCASE}_y${y}.${freq}_${var}_${region}

   if [ ! -f $figs/$g.png ] ; then
     ( ln -sf $f $ff
      python_plot.py -i $g -v $var  -p $pal -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin -units $unit \
            -figsz $width $height -res $res -klev $klev  -pc $charpal  -tick $tick -o $cf_out $orcaopt
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
