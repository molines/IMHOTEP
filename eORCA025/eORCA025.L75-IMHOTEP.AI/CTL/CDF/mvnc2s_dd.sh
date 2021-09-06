#!/bin/bash

if [ $# = 0 ] ; then
   echo " USAGE : mvnc2s_dd.sh  <SEGMENT>"
   echo 
   echo "    PURPOSE : copy reconstructed field from XIOs to STORE"
   exit 0
fi

CONFIG=eORCA025.L75
CASE=IMHOTEP.AI

seg=$1


CONFCASE=${CONFIG}-${CASE}

srcdir=$DDIR/${CONFCASE}-XIOS.$seg
tgtdir=$SDIR/${CONFIG}/${CONFCASE}-S/
cd $srcdir
tmp=( $(ls -1d *_OUTPUT) )

nbfrq=${#tmp[@]}
echo -n $nbfrq " : "

for n in $( seq 0 $(( nbfrq - 1 )) ) ; do
  frqlst[$n]=${tmp[$n]%_OUTPUT}
done

echo ${frqlst[@]}

cd ${frqlst[0]}_OUTPUT
#look for actual year
tmp=$( ls -l ${CONFCASE}_y*.nc | head -1 | awk -F_ '{print $2}' )
year=${tmp:1:4}
echo $year
for freq in ${frqlst[@]} ; do
    mkdir -p  $tgtdir/$freq/$year
    cd $srcdir/${freq}_OUTPUT
    for f in ${CONFCASE}_y${year}*.nc ; do
       dd bs=600M if=$f of=$tgtdir/$freq/$year/$f
    done
done

