#!/bin/bash

#set -x

get_year_list () {

  tmp="$(ls -1 fdbk_dt_global_${typ}_phy_l3_*_*.nc | awk -F_ '{ print $7}' ) "

  (for y in $tmp ; do
    echo ${y:0:4}
  done ) | sort -u

                 }

get_month_list () {
   year=$1
   tmp="$( ls -1 fdbk_dt_global_${typ}_phy_l3_${year}????_*.nc | awk -F_ '{ print $7}' ) "
   ( for y in $tmp ; do
     echo ${y:4:2}
     done )  | sort -u 
                  }

TYP_LIST="tp j1 j2 j3"


n=0
for typ in $TYP_LIST ; do
   cd $typ
   YEAR_LIST=$( get_year_list ) 
   for y in ${YEAR_LIST} ; do
      MONTH_LIST="$( get_month_list $y )"
      echo $typ $y $MONTH_LIST
      ( for m in $MONTH_LIST ; do
        ncrcat -h fdbk_dt_global_${typ}_phy_l3_${y}${m}??_*.nc fdbk_dt_global_${typ}_phy_l3_${y}${m}_20220615.nc
      done ) &
      n=$(( n + 1 ))
      if [ $n = 12 ] ; then
        wait
        n=0
      fi
   done
   wait
   n=0
   cd ../
    

done


exit
fdbk_dt_global_j2_phy_l3_20100904_20210603.nc
