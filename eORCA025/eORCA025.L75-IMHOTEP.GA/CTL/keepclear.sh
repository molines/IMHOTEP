#!/bin/bash

# functions
##  give usage of the script
usage() {
   echo "USAGE   :  keepclean.sh YEAR-deb YEAR-end "
   echo "PURPOSE : Postprocess the range of years given in args"
   echo "          during a production run. This script is launched"
   echo "          from CTL. Different tasks are chained in the script"
   echo "          (1) save the nc files on scratch from XIOS directory (interactif) "
   echo "          (2) Compute annual mean from monthly files (batch)"
   echo "          (3) Concatenate daily files (batch)"
   echo "          (4) Concatenate hourly files (batch)"
   echo "          (5) Post-process observation files"
   echo "          (6) Post-process icebergs files"
   echo "          No automatic cleaning performed ! "
   exit 0
        }

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



# Main
if [ $# != 2 ] ; then
   usage
fi


get_confcase
# log all instance of keepclear
echo $0 $* >> ${CONFCASE}_keepclear.log

# ensure to be in CTL dir
CTLDIR=$PDIR/RUN_${CONFIG}/${CONFCASE}/CTL
DBFILE=${CONFCASE}.db
cd $CTLDIR

#  read years to process
y1=$1
y2=$2

# look for corresponding segments
tmp=$( cat $DBFILE | grep -w ${y1}0131 ) 
if [ $? != 0 ] ; then
   echo " **** ERROR : year $y1 not found in $DBFILE"
   exit 1
fi

seg1=$( echo $tmp | awk '{print $1}' )

tmp=$( cat $DBFILE | grep -w ${y2}1231 )
if [ $? != 0 ] ; then
   echo " **** ERROR : year $y2 not found in $DBFILE"
   exit 1
fi

seg2=$( echo $tmp | awk '{print $1}' )

echo " Period $y1 ==> $y2 corresponds to segments $seg1 ==> $seg2 "

# (1 ) Ready for dealing with XIOS
if [ ! -f dcmtk_mvnc2s_mvfast ] ; then
    echo " **** ERROR : missing dcmtk_mvnc2s_mvfast script in $CTLDIR"
    exit 2
else
    cp dcmtk_mvnc2s_mvfast $DDIR/
fi

cd $DDIR
chmod 755 ./dcmtk_mvnc2s_mvfast
for n in $( seq $seg1 $seg2 ) ; do
   XIOSDIR=${CONFCASE}-XIOS.$n
   if [ ! -d $XIOSDIR ] ; then
     echo " **** ERROR : missing $XIOSDIR "
     exit 3
   fi
   cd $XIOSDIR
   ../dcmtk_mvnc2s_mvfast
   cd $DDIR
done

# Now proceed with launching batches from CTL
cd $CTLDIR
# (2 ) Annual mean

 if [ ! -f jobannual ] ; then
    echo " **** ERROR : missing jobannual "
    exit 4 
 fi
    sbatch ./jobannual $( seq $y1 $y2 ) 

# (3) Concatenate daily files
 if [ !  -f concat1d_multi.sh ] ; then
    echo " **** ERROR : missing concat1d_multi.sh"
    exit 5
 fi
    sbatch ./concat1d_multi.sh  $( seq $y1 $y2 )


# (4) Concatenate hourly files
 if [ !  -f concat1d_multi_1h.sh ] ; then
    echo " **** ERROR : missing concat1d_multi_1h.sh"
    exit 5
 fi
    sbatch ./concat1d_multi_1h.sh  $( seq $y1 $y2 )

# (5) Post process OBS files
 if [ ! -f jobobs_2 ] ; then  
    echo " **** ERROR : missing jobobs_2"
    exit 6
 fi
    sbatch ./jobobs_2 $CONFCASE $seg1 $seg2

# (6) Post process ICB files
 if [ ! -f jobtrj_2 ] ; then  
    echo " **** ERROR : missing jobtrj_2"
    exit 7
 fi
   sbatch ./jobtrj_2 $CONFCASE $seg1 $seg2
