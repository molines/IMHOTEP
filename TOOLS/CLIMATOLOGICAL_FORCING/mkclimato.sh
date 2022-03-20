#!/bin/bash

# This script consume a lot of memory
# It is not parrallel and is killed for OOM (Out of Memory) in batch when using a shared node ...
# I ran it interactively without problems (and rather fast).

ulimit -s unlimited

### ----- set global variables according to your preferences ---- ###
###         note that DEVGIT points to the directory where you cloned IMHOTEP repository
DATA_FORCING=$WORKDIR/DATA_FORCING
DATA_SET=JRA55
   prefix=drowned_    # files are named <prefix><var>_<DATA_SET>_y<YEAR>.nc
   # input var names
   u10=uas     # name of u10 in this data set
   v10=vas     # name of v10 in this data set
   # output var names
   w10=w10     # name of windspeed (module) in the output file
   wu10=wu10   # name of W10 x U10 variable (output)
   wv10=wv10   # name of W10 x V10 variable (output)

DATA_DIR_3H=drowned
WORK_DIR_3H=$DATA_FORCING/$DATA_SET/TMP_CLIMATO
FORCING_TOOLS=$DEVGIT/IMHOTEP/TOOLS/CLIMATOLOGICAL_FORCING/bin
year_span=1980-2019 # year range for climatology computation
CLIM_DIR=$DATA_FORCING/$DATA_SET/CLIMATO_$year_span
###-----------------------------------------------------------------------###

if [ ! -x $FORCING_TOOLS/mkclimato.exe ] ; then
   echo " Please compile forcing tools before using this script "
   echo "   cd $FORCING_TOOLS/../src"
   echo "   make "
   exit
fi

y1=$(echo $year_span | awk -F- '{print $1}' )
y2=$(echo $year_span | awk -F- '{print $2}' )

mkdir -p $CLIM_DIR

# move to working directory
cd $WORK_DIR_3H

# set list of variables to process taking the first year
for f in ${prefix}*_${DATA_SET}_y${y1}.nc ; do
  lst="$lst $(echo $f | awk -F_ '{print $2}' ) "
done

# process all variables except u10 v10 which are not used
for var in $lst ; do
   if [ $var != $u10 -a  $var != $v10] ; then
      echo "Processing variable : $var "
      
      g=${prefix}${var}_${DATA_SET}_CLIM_${year_span}.nc
      gf=${prefix}${var}_${DATA_SET}_CLIM_${year_span}-f.nc
      if [ ! -f $CLIM_DIR/$gf ] ; then
        echo "Processing variable : $var "
        $FORCING_TOOLS/mkclimato.exe ${prefix}${var}_${DATA_SET}_y????.nc
        mv climato.nc $CLIM_DIR/$g      # unfiltered files
        mv climato-f.nc $CLIM_DIR/$gf   # 10 days filtered
      else
        echo "Variable : $var  already processed !"
      fi
   fi
done
