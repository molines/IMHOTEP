#!/bin/bash

## determine CONFCASE from the actual directory (CTL or CDF)
get_confcase() {
here=$(pwd)
b_name=$( basename $here )

case $b_name in
(CTL) tmp=$(dirname  $here )
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

# look for confcase
get_confcase

y1=2015
y2=2018

MWDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN
MSDIR=$DDIR/${CONFIG}/${CONFCASE}-S

for freq in 1m  1y; do
  cd $MSDIR/$freq
  for y in $( seq $y1 $y2 ) ; do
     mkdir -p $MWDIR/$freq/$y
     cd $y
        for f in ${CONFCASE}_y${y}*.nc ; do
           echo -n "transfering  $f ..."
           dd bs=600M if=$f of=$MWDIR/$freq/$y/$f
           echo "... done."
        done 
     cd ../
  done
done

# create monthly mean links in 1y for monitoring ...
cd $MWDIR/1y/
for y in $( seq $y1 $y2 ) ; do
   cd $y
   for f in *.nc ; do
     TYP=$(echo ${f%.nc} | awk -F_ '{print $3}' )
     for m in {01..12} ; do
       f1m=$MWDIR/1m/$y/${CONFCASE}_y${y}m${m}.1m_${TYP}.nc
       f1y=${CONFCASE}_y${y}m${m}.1y_${TYP}.nc
       if [ -f $f1m ] ; then
         ln -sf $f1m ./$f1y
       else
         echo $f1m missing
       fi
       # <CONFCASE>_<YEAR>.<freq>_<TYP>.nc
     done
   done
   cd ../
done
