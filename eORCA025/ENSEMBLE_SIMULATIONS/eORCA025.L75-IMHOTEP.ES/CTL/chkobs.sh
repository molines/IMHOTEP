#!/bin/bash
CONFIG=eORCA025.L75
CASE=IMHOTEP.ES
nmember=10
lverbose=1
CONFCASE=${CONFIG}-${CASE}

SSDIRO=$DDIR/${CONFIG}/${CONFCASE}-S/OBS
SSDIRT=$DDIR/${CONFIG}/${CONFCASE}-S/ICBTRJ

cd $SSDIRO

cfile1=$(ls -l ${CONFIG}* |  sort -t. -k5n |head -2 | tail -1 | awk '{print $9}')
cfilen=$(ls -l ${CONFIG}* |  sort -t. -k5n | tail -1 | awk '{print $9}')

seg1=$( echo ${cfile1%.nc} | awk -F. '{print $NF}' )
segn=$( echo ${cfilen%.nc} | awk -F. '{print $NF}' )

echo OBS : Segments $seg1 to $segn processed

for seg in $( seq $seg1 $segn ) ; do
   nf=$( ls -l ${CONFIG}*.$seg.nc | wc -l )
   if [ $nf != $nmember ]; then
     echo " ************  WARNING :  ONLY  $nf members ******* "
   fi
   echo Segment $seg : $nf  members
   if [ $lverbose = 1 ] ; then
   for mbr in $(seq 1 $nf) ; do
     MBR=$( printf "%03d" $mbr)
     NOBS[$mbr]=$( ncdump -h ${CONFIG}*.${MBR}*.$seg.nc | head -3 | tail -1 | awk '{ print $(NF-1) }' )
     echo "      member $MBR :  ${NOBS[$mbr]} Observations" 
   done
   fi
done

cd $SSDIRT
cfile1=$(ls -l ${CONFIG}* |  sort -t. -k5n |head -2 | tail -1 | awk '{print $9}')
cfilen=$(ls -l ${CONFIG}* |  sort -t. -k5n | tail -1 | awk '{print $9}')

seg1=$( echo ${cfile1%.nc} | awk -F. '{print $NF}' )
segn=$( echo ${cfilen%.nc} | awk -F. '{print $NF}' )

echo ICEBERGS : Segments $seg1 to $segn processed

for seg in $( seq $seg1 $segn ) ; do
   nf=$( ls -l ${CONFIG}*.$seg.nc | wc -l )
   if [ $nf != $nmember ]; then
     echo " ************  WARNING :  ONLY  $nf members ******* "
   fi
   echo Segment $seg : $nf  members
   if [ $lverbose = 1 ] ; then
   for mbr in $(seq 1 $nf) ; do
     MBR=$( printf "%03d" $mbr)
     NTRJ[$mbr]=$( ncdump -h ${CONFIG}*.${MBR}*.$seg.nc | head -3 | tail -1 | awk '{ print $(NF-1) }' )
     echo "      member $MBR :  ${NTRJ[$mbr]} trajectories " 
   done
   fi
done

