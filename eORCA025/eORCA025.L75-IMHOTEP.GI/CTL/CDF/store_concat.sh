#!/bin/bash
#SBATCH -J JOB_store
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH -A bcn@cpu
#SBATCH --time=2:30:00
#SBATCH -e zjobstore.e%j
#SBATCH -o zjobstore.o%j

if [ $# = 0 ] ; then
   echo " USAGE : "
   echo "    store_concat.sh  LST-years"
   echo
   echo " PURPOSE : "
   echo "    Transfert 1d and 1h concatenated files from scratch to store "
   echo "    This script also transfers 1m and 1y files "
   echo "    Note that 1y files are computed from 1m files with jobannual"
   echo
   exit 0
fi

# some usefull funcion
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
      exit 1 ;;
  esac
echo "     CONFIG   : " $CONFIG
echo "     CASE     : " $CASE
echo " ==> CONFCASE : " $CONFCASE
               }
get_bs() {
     tmp=$( ls -lShr ${CONFCASE}_y${year}*${freq}_*.nc| tail -1 | awk '{print $5}' )
     nbs=${#tmp}
     nbs=$(( nbs - 1 )) # skip last letter 
     unit=${tmp:$nbs:1}
     sz=${tmp:0:$nbs}
     BS=$( echo $sz | awk '{ print int($1)+1 }' )$unit
         }

get_confcase

STORE_ROOT=$SDIR
SRC_ROOT=$DDIR

lstyr=( $* )
nyears=${#lstyr[@]}
echo "Number of years to tranfer : $nyears"

STODIR=$STORE_ROOT/${CONFIG}/${CONFCASE}-S
SRCDIR=$SRC_ROOT/${CONFIG}/${CONFCASE}-S

for year in ${lstyr[@]} ; do
   for freq in 1d 1h ; do
     tgtdir=$STODIR/$freq/${year}-concat
     srcdir=$SRCDIR/$freq/${year}-concat

     mkdir -p $tgtdir
     cd $srcdir
#     BS=$( ls -lShr ${CONFCASE}_y${year}m??_${freq}_*.nc| tail -1 | awk '{print $5}' )
     get_bs
     for f in ${CONFCASE}_y${year}m??_${freq}_*.nc ; do
       echo $f
        dd bs=$BS if=$f of=$tgtdir/$f
     done
   done
   for freq in 1m 1y ; do
     tgtdir=$STODIR/$freq/${year}
     srcdir=$SRCDIR/$freq/${year}

     mkdir -p $tgtdir
     cd $srcdir
#     BS=$( ls -lShr ${CONFCASE}_y${year}*.${freq}_*.nc  | tail -1 | awk '{print $5}' )
     get_bs
     for f in ${CONFCASE}_y${year}*.${freq}_*.nc ; do
       echo $f
       dd bs=$BS if=$f of=$tgtdir/$f
     done
   done
done
