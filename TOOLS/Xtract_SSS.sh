#!/bin/bash


#  Xtract SSS at specific location for interannual variability check
CONFIG=eORCA025.L75
CASE=IMHOTEP.GAI
freq=1m
var=sosaline
y1=1997
y2=2007

#-------------------------------
CONFCASE=${CONFIG}-${CASE}

MESH_MASK=$WORK/${CONFIG}/${CONFIG}-I/${CONFIG}_mesh_mask_closed_seas_greenland.nc
MWDIR=$WORK/${CONFIG}/${CONFCASE}-MEAN/$freq

# working space on scratch 
WKDIR=$DDIR/ZWK_$(basename $0)
mkdir -p $WKDIR
cd $WKDIR
# Build list of points
cat << eof > zsss_points.txt
156  0
165  0
180  0
-125 0
165 5
165 2
165 -2
165 -5
100 -7
55 -1
eof

cdffindij -f zsss_points.txt -c $MESH_MASK -p T -A -l -o ztmp.txt

npoint=$( cat ztmp.txt | wc -l )
for y in $( seq $y1 $y2 ) ; do
   for f in $MWDIR/$y/${CONFCASE}_y${y}m??.${freq}_gridTsurf.nc ; do
      tag=$( echo $( basename $f ) | awk -F_ '{print $2}' )
      for np in $( seq 1 $npoint) ; do
        lon=$( tail -$np ztmp.txt | head -1  | awk '{print $1}' )
        lat=$( tail -$np ztmp.txt | head -1  | awk '{print $2}' )
        if [ $lon -lt  0 ] ; then
            LON=${lon:1}W
        else
            LON=${lon}E
        fi
        if [ $lat -lt  0 ] ; then
            LAT=${lat:1}S
        else
            LAT=${lat}N
        fi
        fname=${CONFCASE}_${var}_${LON}-${LAT}.txt
        echo $fname
        ilook=$( tail -$np ztmp.txt | head -1  | awk '{print $5}' )
        jlook=$( tail -$np ztmp.txt | head -1  | awk '{print $6}' )

        cdfprobe -f $f -v $var -i $ilook -j $jlook  >> $fname
      done
   done
done

