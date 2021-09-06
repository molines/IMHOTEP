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
#SBATCH --time=20:00:00
#SBATCH --exclusiv
    
#  /gpfswork/rech/bcn/rcli002/eORCA025.L75/eORCA025.L75-IMHOTEP.GAI-MEAN/1y/1980


CONFIG=eORCA025.L75
CASE=IMHOTEP.GAI
freq=1y
y1=2009
y2=2018
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
     



