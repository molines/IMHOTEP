#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A bcn@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_NAN
#SBATCH -e zjobnan.e%j
#SBATCH -o zjobnan.o%j
#SBATCH --time=2:30:00
#SBATCH --exclusive

list="PRODU PRODV gridU gridV gridT"
nmax=40

if [ $# -ne 2 ] ; then
   echo "USAGE :  killnan YEAR-deb YEAR-end"
   echo "PURPOSE : Run cdfnan on 3D files $list in year directories"
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

cd $DDIR/${CONFIG}/${CONFCASE}-S/1y

for typ in $list  ; do
   n=0
  for y in $(seq $y1 $y2 )  ; do
     cd $y
     f=${CONFCASE}_y${y}.1y_$typ.nc
     echo "Processing $f "
     cdfnan -l $f &
     n=$(( n + 1 ))
     cd ../
     if [ $n = $nmax ] ; then
       wait
       echo " ... done "
       n=0
     fi
  done
       echo " ... done "
wait
done
wait
