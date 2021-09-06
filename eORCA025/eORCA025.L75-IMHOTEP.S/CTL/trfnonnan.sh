#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=21
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A bcn@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_ST
#SBATCH -e zjobst.e%j
#SBATCH -o zjobst.o%j
#SBATCH --time=2:30:00
#SBATCH --exclusive

list="PRODU PRODV gridU gridV gridT"
nmax=40

if [ $# -ne 2 ] ; then
   echo "USAGE :  trfnonnan  YEAR-deb YEAR-end"
   echo "PURPOSE : transfer repaired files to SDIR and WORKDIR "
   exit 0
fi

y1=$1
y2=$2
ny=$(( y2 - y1 + 1 ))
get_confcase() {
here=$(pwd)
b_name=$( basename $here )

case $b_name in
(CTL) tmp=$(dirname  $here )
      echo "paf $tmp "
      CONFCASE=$( basename $tmp )
      CONFIG=${CONFCASE%-*}
      CASE=${CONFCASE#*-} ;;

(CDF ) tmp=$(dirname $here  )
       tmp=$(dirname $tmp  )
      CONFCASE=$( basename $tmp )
      CONFIG=${CONFCASE%-*}
      CASE=${CONFCASE#*-} ;;
( * ) echo " cannot infer CONFASE !"
      echo " This is not a CTL dir nor a CTL/CDF dir "
      exit 1
esac
echo "     CONFIG   : " $CONFIG
echo "     CASE     : " $CASE
echo " ==> CONFCASE : " $CONFCASE
               }

get_confcase
SYDIR=$SDIR/${CONFIG}/${CONFCASE}-S/1y
WYDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN/1y

for typ in $list ; do
   n=0
  for y in $( seq $y1 $y2 ) ; do
     cd $y
     f=${CONFCASE}_y${y}.1y_$typ.nc
       dd bs=600M if=$f of=$SYDIR/$y/$f &
       dd bs=600M if=$f of=$WYDIR/$y/$f &
     n=$(( n + 2 ))
     cd ../
     if [ $n -ge $nmax   ] ; then
       wait
       n=0
     fi
  done
wait
done
wait
