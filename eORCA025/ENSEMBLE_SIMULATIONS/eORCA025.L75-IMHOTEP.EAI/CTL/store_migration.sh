#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=10
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A cli@cpu
#SBATCH --hint=nomultithread
#SBATCH -J STORMIG
#SBATCH -e zstomig.e%j
#SBATCH -o zstomig.o%j
#SBATCH --time=2:00:00
##SBATCH --exclusive


set -x


CONFIG=eORCA025.L75
CASE=IMHOTEP.EAI

freq=5d

ylst=( {2017..2018} )
mlst=( {001..010} )
n=0
#==================
case $freq in 
(1y)
for mbr in ${mlst[@]} ; do
  CONFCASE=${CONFIG}-${CASE}.${mbr}
  TGTDIR=$SDIR/${CONFIG}/$CONFCASE-S
  SRCDIR=$DDIR/${CONFIG}/$CONFCASE-S
  mkdir -p $TGTDIR/$freq

  for y in ${ylst[@]} ; do 
    cd $SRCDIR/$freq/
    rsync -arv $y $TGTDIR/$freq
  done
done ;;
(1m  ) 
for mbr in ${mlst[@]} ; do
  ( CONFCASE=${CONFIG}-${CASE}.${mbr}
  TGTDIR=$SDIR/${CONFIG}/$CONFCASE-S
  SRCDIR=$DDIR/${CONFIG}/$CONFCASE-S
  for  y in ${ylst[@]} ; do
     mkdir -p $TGTDIR/$freq/$y
     cd $SRCDIR/$freq/$y
     for f in *.nc ; do
       dd bs=3G if=$f of=$TGTDIR/$freq/$y/$f
     done
  done ) &
  n=$(( n + 1 ))
  if [ $n = 10 ] ; then
    n=0
    wait
  fi

done ;;

( 1d | 5d ) 
for mbr in ${mlst[@]} ; do
  ( CONFCASE=${CONFIG}-${CASE}.${mbr}
  TGTDIR=$SDIR/${CONFIG}/$CONFCASE-S
  SRCDIR=$DDIR/${CONFIG}/$CONFCASE-S
  for  y in ${ylst[@]} ; do
     mkdir -p $TGTDIR/$freq/${y}-concat
     cd $SRCDIR/$freq/${y}-concat
     for f in *.nc ; do
       dd bs=3G if=$f of=$TGTDIR/$freq/${y}-concat/$f
     done
  done ) &
  n=$(( n + 1 ))
  if [ $n = 10 ] ; then
    n=0
    wait
  fi

done ;;

esac

wait

