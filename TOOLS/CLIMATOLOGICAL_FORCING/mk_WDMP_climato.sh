#!/bin/bash

# This script consume a lot of memory
# It is not parrallel and is killed for OOM (Out of Memory) in batch when using a shared node ...
# I ran it interactively without problems (and rather fast).

ulimit -s unlimited

### ----- set global variables according to your preferences ---- ###
###         note that DEVGIT points to the directory where you cloned IMHOTEP repository
DATA_FORCING=$WORKDIR/DATA_FORCING
DATA_SET=JRA55
DATA_DIR_3H=drowned
CDF_TOOLS=$DEVGIT/CDFTOOLS/bin
year_span=1980-2019 # year range for climatology computation
CLIM_DIR=$DATA_FORCING/$DATA_SET/CLIMATO_$year_span
CONFREF=eORCA025.L75-IMHOTEP02   # reference config used to compute SSS restoring term
###-----------------------------------------------------------------------###

if [ ! -x $CDF_TOOLS/cdfmoyt ] ; then
   echo " Please compile cdftools  before using this script "
   exit
fi

y1=$(echo $year_span | awk -F- '{print $1}' )
y2=$(echo $year_span | awk -F- '{print $2}' )

mkdir -p $CLIM_DIR

# move to 3H directory (where monthly WDMP file are linked ... )
cd $DATA_FORCING/$DATA_SET/$DATA_DIR_3H

#set list of file to deal with
lst=''
for y in $( seq $y1 $y2 ) ; do
  lst="$lst ${CONFREF}_WDMP_y$y.nc"
done
# compute monthly climatology
if [ ! -f $CLIM_DIR/${CONFREF}_WDMP_CLIM_${year_span}.nc ] ; then
    $CDF_TOOLS/cdfmoyt -l $lst -nc4 -o $CLIM_DIR/${CONFREF}_WDMP_CLIM_${year_span}
    rm $CLIM_DIR/${CONFREF}_WDMP_CLIM_${year_span}2.nc
     echo "   ${CONFREF}_WDMP_CLIM_${year_span}.nc computed !"
else
     echo "   ${CONFREF}_WDMP_CLIM_${year_span}.nc exists ! Nothing more to do !"
fi


