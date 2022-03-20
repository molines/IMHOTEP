#!/bin/bash

# this script remove Feb 29 from leap year files.

# need cdo
module load cdo

### ----- set global variables according to your preferences ---- ###
###         note that DEVGIT points to the directory where you cloned IMHOTEP repository
DATA_FORCING=$WORKDIR/DATA_FORCING
DATA_SET=JRA55
   prefix=drowned_    # files are named <prefix><var>_<DATA_SET>_y<YEAR>.nc
DATA_DIR_3H=drowned
WORK_DIR_3H=$DATA_FORCING/$DATA_SET/TMP_CLIMATO

year_span=1980-2019 # year range for climatology computation

###-----------------------------------------------------------------------###

isleap()  {
   # return 1 if argument is multiple of 4 
   # note that century year are not leap unless
   # they are multiple of 400 .. next century year not leap is 2100 
   if [ $(( $1 % 4 )) = 0 ] ; then
     if [ $(( $1 % 100 )) = 0 ] ; then
        if [ $(( $1 % 400 )) = 0 ] ; then
          echo 1  
        fi
     else
       echo 1
     fi
   fi
          }
    
y1=$(echo $year_span | awk -F- '{print $1}' )
y2=$(echo $year_span | awk -F- '{print $2}' )

# first deal with w10 wu10 and wv10 files in WORK_DIR_3H
cd $WORK_DIR_3H/
for y in $( seq $y1 $y2 ) ; do
    if [ $( isleap $y ) ] ; then
       for f in ${prefix}w*10_${DATA_SET}_y$y.nc ; do
          echo removing Feb 29 in $f
          cdo del29feb $f $f.no29feb
       done
    fi
done
# rename no29feb file to original name (WORK_DIR_3H !!! )
 rename .nc.no29feb  .nc  *.no29feb

#  Then  deal with files in 3H directory
#  resulting files are put in WORK_DIR_3H (same name !!! )
cd $DATA_FORCING/$DATA_SET/$DATA_DIR_3H
for y in $( seq $y1 $y2 ) ; do
    if [ $( isleap $y ) ] ; then
       for f in ${prefix}*_${DATA_SET}_y$y.nc ; do
          echo removing Feb 29 in $f
          cdo del29feb $f $WORK_DIR_3H/$f 
       done
    fi
done

echo "All ${prefix}'*' files are noleap in $WORK_DIR_3H"
