#!/bin/bash

for typ in tp j1 j2 j3 ; do

   for f in ../$typ/fdbk_dt_global_${typ}_phy_l3_*_*.nc ; do
      echo $f
      tmp=$( basename $f )
      g=$( echo ${tmp%_*} | sed -e "s/$typ/SAT/" ).nc
      ln -sf $f $g
   done
done
