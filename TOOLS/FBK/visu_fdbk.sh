#!/bin/bash

  if [ $# = 0 ] ; then
     echo "USAGE :  visu_fdbk.sh  FDBK-file"
     echo " "
     echo "PURPOSE: "
     echo "     Just draw a map of all the observation points in the FDBK-file"
     echo 
     exit
  fi
set -x

fdbk=$1

 ncks -F -H -s "%f \n"  -v LONGITUDE $fdbk > zlon${fdbk%.nc}.txt
 ncks -F -H -s "%f \n"  -v LATITUDE  $fdbk > zlat${fdbk%.nc}.txt
 paste zlon${fdbk%.nc}.txt zlat${fdbk%.nc}.txt >  zlonlat${fdbk%.nc}.txt

export BITMAPSIZE=1800x1024

cat zlonlat${fdbk%.nc}.txt  | graph -TX -m -2 -S 1

rm zlon*.txt zlat*.txt

