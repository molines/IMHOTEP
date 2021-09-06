#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
####SBATCH --partition=prepost
#SBATCH -A bcn@cpu
#SBATCH --hint=nomultithread
#SBATCH -J CONCAT_
#SBATCH -e zconcat.e%j
#SBATCH -o zconcat.o%j
#SBATCH --time=5:00:00
#SBATCH --exclusive


# script used to concatenate daily output into monthly files (with 28 29 30 or 31 time frames in it)
CONFIG=eORCA025.L75
CASE=IMHOTEP.AI
#
freq=1d # not to be changed !

CONFCASE=${CONFIG}-${CASE}
STORE_ROOT=$DDIR
DTADIR=$STORE_ROOT/${CONFIG}/${CONFCASE}-S/$freq

if [ $# = 0 ] ; then
   echo " USAGE : concat1d YEAR-list "
   echo "   "
   echo " PURPOSE :  scan the 1d output directory for YEAR in YEAR-list, find the different types of file "
   echo "            (gridT gridU ICB .... etc ) and concatenate each set of daily files for "
   echo "            a month into a single file."
   echo "           CONFIG and CASE are hard coded in the header part of the script"
   echo "              actual CONFIG = "$CONFIG
   echo "              actual CASE   = "$CASE
   echo "           This program work on STORE by default:"
   echo "           directory :"$DTADIR"/YEAR"
   exit 0
fi

years=( $* )

for year in ${years[@]} ; do
wkdir=$DTADIR/$year
cd $wkdir 
# look for list of type to concatenate
lst=( $(ls -1 *m01d01*nc | awk -F_ '{print $NF}' | sed -e 's/.nc//' ) )


# check that there are 12 month for each type
ierr=0
for typ in ${lst[@]} ; do
   nf=$(ls ${CONFCASE}_y${year}m??d??.${freq}_$typ.nc | wc -w )
   echo "type :" $typ " : " $nf " files."
   if [ $nf -lt 365 ] ; then
     echo " ####   PROBLEM for $typ : ONLY $nf files ####"
     ierr=$(( ierr + 1 ))
   fi
done
echo $ierr

done


# concatenate
if [ $ierr = 0 ] ; then
   for typ in ${lst[@]} ; do
   for year in ${years[@]} ; do
     wkdir=$DTADIR/$year
     cd $wkdir 
     mkdir -p $DTADIR/${year}-concat
     for m in {01..12} ; do
       ncrcat ${CONFCASE}_y${year}m${m}d??.${freq}_$typ.nc  $DTADIR/${year}-concat/${CONFCASE}_y${year}m${m}_${freq}_$typ.nc &
     done
   done
   wait
   done

else
   echo incorrect number of files somehow
   exit 1
fi




