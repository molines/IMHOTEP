#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
##SBATCH --partition=prepost
#SBATCH -A cli@cpu
#SBATCH --hint=nomultithread
#SBATCH -J CONCAT5d_2_eai
#SBATCH -e zconcat.e%j
#SBATCH -o zconcat.o%j
#SBATCH --time=2:00:00
#SBATCH --exclusive


# script used to concatenate daily output into monthly files (with 28 29 30 or 31 time frames in it)
CONFIG=eORCA025.L75
CASE=IMHOTEP.EAI
#
freq=5d # not to be changed !

STORE_ROOT=$DDIR

if [ $# = 0 ] ; then
   echo " USAGE : concat5d_2 YEAR  "
   echo "   "
   echo " PURPOSE :  scan the 5d output directory for YEAR, find the different types of file "
   echo "            (gridT gridU ICB .... etc ) and concatenate each set of daily files for "
   echo "            a month into a single file for all members."
   echo "           CONFIG and CASE are hard coded in the header part of the script"
   echo "              actual CONFIG = "$CONFIG
   echo "              actual CASE   = "$CASE
   echo "           This program work on STORE by default:"
   echo "           directory :"$DTADIR"/YEAR"
   exit 0
fi

year=$1
mbrlst=( {001..010})


# check that there are 73  file  for each type
ierr=0
for mbr in ${mbrlst[@]} ; do
    CONFCASE=${CONFIG}-${CASE}.${mbr}
    DTADIR=$STORE_ROOT/${CONFIG}/${CONFCASE}-S/$freq
    wkdir=$DTADIR/$year
    cd $wkdir 
    # look for list of type to concatenate
    lst=( gridW  )

    for typ in ${lst[@]} ; do
       nf=$(ls ${CONFCASE}_y${year}m??d??.${freq}_$typ.nc | wc -w )
       echo "type :" $typ " : " $nf " files. mbr "$mbr
       if [ $nf -lt 73 ] ; then
       echo " ####   PROBLEM for $typ : ONLY $nf files for mbr $mbr ####"
       ierr=$(( ierr + 1 ))
       fi
   done
done
echo $ierr

# concatenate
if [ $ierr = 0 ] ; then
   for typ in ${lst[@]} ; do
       for mbr in ${mbrlst[@]} ; do
         CONFCASE=${CONFIG}-${CASE}.${mbr}
         DTADIR=$STORE_ROOT/${CONFIG}/${CONFCASE}-S/$freq
         mkdir -p $DTADIR/${year}-concat
         wkdir=$DTADIR/$year
         cd $wkdir 
         ncrcat  ${CONFCASE}_y${year}m??d??.${freq}_$typ.nc  $DTADIR/${year}-concat/${CONFCASE}_y${year}_${freq}_$typ.nc &
       done
       wait
   done
   wait

else
   echo incorrect number of files somehow
   exit 1
fi




