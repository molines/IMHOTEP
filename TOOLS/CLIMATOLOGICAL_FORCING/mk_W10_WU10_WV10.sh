#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A cli@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=1:00:00
#SBATCH --exclusive

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
max_par_task=40     # max parallel task (should be coherent with  --ntasks in the batch header!

###-----------------------------------------------------------------------###

if [ ! -x $FORCING_TOOLS/mkw10.exe -o -x $FORCING_TOOLS/mkmodxu.exe ] ; then
   echo " Please compile forcing tools before using this script "
   echo "   cd $FORCING_TOOLS/../src"
   echo "   make "
   exit
fi
  
y1=$(echo $year_span | awk -F- '{print $1}' )
y2=$(echo $year_span | awk -F- '{print $2}' )

cd $DATA_FORCING/$DATA_SET
mkdir -p $WORK_DIR_3H


cd $DATA_DIR_3H
n=0

for y in $( seq $y1 $y2 ) ; do
   $FORCING_TOOLS/mkw10.exe -y y${y} -set $DATA_SET -u10 $u10 -v10 $v10 -w10 $w10 -prefix $prefix &
   n=$(( n + 1 ))
   if [ $n = $max_par_task ] ; then
     wait
     n=0
   fi
done
wait

mv ${prefix}${w10}*.nc $WORK_DIR_3H

for y in $( seq $y1 $y2 ) ; do
   $FORCING_TOOLS/mkmodxu.exe -y y${y} -set $DATA_SET -u10 $u10 -v10 $v10 -wu10 $wu10  -wv10 $wv10 -prefix $prefix &
   n=$(( n + 1 ))
   if [ $n = $max_par_task ] ; then
     wait
     n=0
   fi
done
wait
mv ${prefix}${wu10}*.nc $WORK_DIR_3H
mv ${prefix}${wv10}*.nc $WORK_DIR_3H
