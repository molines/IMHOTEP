#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=24
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A bcn@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=2:00:00
#SBATCH --exclusive
    
usage()   {
   echo
   echo "USAGE:  mk_decadal_clim.sh -c CONFIG-CASE -f freq -p period [-n]"
   echo 
   echo "   PURPOSE: "
   echo "       Compute multiannual (not necessarly decadal) monthly climatology"
   echo "     from already computed monthly means."
   echo "     This script will compute the multi annual climatology for all file types"
   echo "     that are available in the first year of the period."
   echo
   echo "   OPTIONS:"
   echo "     -c CONFIG-CASE : give the CONFIG-CASE name of the config to work with."
   echo "     -f freq : give the frequency (1d 1m 1y ... ) used to identify the files in"
   echo "              the CONFIG-CASE-MEAN/ directory."
   echo "     -p period : period indicates first and last year of the period to take into"
   echo "              account in the climatology. It corresponds to 2 years separated by"
   echo "              a dash, for example : -p 2009-2018 "
   echo "    -n  : just a dry run to check the options."
   echo "   "
   exit 0
         }
dryrun=0

if [ $# = 0 ] ; then usage ; fi
while getopts :hc:f:p:n opt ; do
  case $opt in
   (h)  usage ;;
   (c)  CONFCASE=$OPTARG ;;
   (f)  freq=$OPTARG ;;
   (p)  period=$OPTARG ;;
   (n)  dryrun=1 ;;
   (\?) echo $( basename $0 ): option -$OPTARG not valid. ; usage ;;
  esac
done

CONFIG=${CONFCASE%-*}
CASE=${CONFCASE#*-}
y1=${period%-*}
y2=${period#*-}

echo "working with :"
echo "   CONFIG = " $CONFIG
echo "   CASE   = " $CASE
echo "   freq   = " $freq
echo "   period = " $period
echo "       y1 = " $y1
echo "       y2 = " $y2

if [ $dryrun = 1 ] ; then
   exit
fi

# --------------------

CONFCASE=${CONFIG}-${CASE}
MWDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN/$freq
mkdir -p $MWDIR/${y1}-${y2}

# Annual climatology
cd $MWDIR/$y1
#look for list
lst=''
for f in ${CONFCASE}_y${y1}.${freq}_*.nc ; do
   typ=$(  echo ${f%.nc} | awk -F_ '{print $NF}' )
   lst="$lst $typ"
done
echo $lst
cd $MWDIR/

n=0
for typ in $lst ; do
  zlst=''
  for y in $( seq $y1 $y2 ) ; do
     zlst="$zlst $y/${CONFCASE}_y${y}.${freq}_${typ}.nc"
  done
    cdfmoy_weighted -l $zlst -o $MWDIR/${y1}-${y2}/${CONFCASE}_y${y1}-${y2}.${freq}_${typ}.nc &
    n=$(( n + 1 ))
    if [ $n = 16 ] ; then
       wait
       n=0
    fi
done
wait
# monthly climatology
cd $MWDIR/$y1
#look for list
lst=''
for f in ${CONFCASE}_y${y1}m01.${freq}_*.nc ; do
   typ=$(  echo ${f%.nc} | awk -F_ '{print $NF}' )
   lst="$lst $typ"
done
echo $lst
cd $MWDIR/
n=0
for typ in $lst ; do
   zlst=''
   for m in {01..12} ; do
     for y in $( seq $y1 $y2 ) ; do
      zlst="$zlst $y/${CONFCASE}_y${y}m${m}.${freq}_${typ}.nc"
     done
     cdfmoy_weighted -l $zlst -o $MWDIR/${y1}-${y2}/${CONFCASE}_y${y1}-${y2}m${m}.${freq}_${typ}.nc &
     n=$(( n + 1 ))
     if [ $n = 24 ] ; then
       wait
       n=0
    fi
   done
   wait
done
