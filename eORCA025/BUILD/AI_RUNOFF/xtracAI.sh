#!/bin/bash
f=../eORCA025.L75_rnf_msk.nc
 echo -n $f ...
 # AO Amazone Orenoque
  echo -n ... AO ...
  zoom=" -d x,880,1020 -d y,650,740"
  g=$( echo $( basename $f)  | sed -e "s/eORCA025/AO/g" )
  ncks -O  $zoom $f  $g

 # NC Niger Congo
  echo -n ... NC ...
  zoom=" -d x,1155,1214 -d y,651,715"
  g=$( echo $( basename $f) | sed -e "s/eORCA025/NC/g")
  ncks -O  $zoom $f  $g

 # GB  Gange Bramapoutre
  echo -n ... GB ...
  zoom=" -d x,48,95 -d y,760,790"
  g=$( echo $( basename $f) | sed -e "s/eORCA025/GB/g" )
  ncks -O  $zoom $f  $g

 # IW  Iriwadi
  echo  ... IW 
  zoom=" -d x,77,104 -d y,739,760"
  g=$( echo $( basename $f) | sed -e "s/eORCA025/IW/g" )
  ncks -O  $zoom $f  $g


 for f in ../*runoff_ISBA* ; do

  echo -n $f ...
 # AO Amazone Orenoque
  echo -n ... AO ...
  zoom=" -d x,880,1020 -d y,650,740"
  g=$( echo $( basename $f) | sed -e "s/eORCA025/AO/g" )
  ncks -O  $zoom $f  $g

 # NC Niger Congo
  echo -n ... NC ...
  zoom=" -d x,1155,1214 -d y,651,715"
  g=$( echo $( basename $f) | sed -e "s/eORCA025/NC/g")
  ncks -O  $zoom $f  $g

 # GB  Gange Bramapoutre
  echo -n ... GB ...
  zoom=" -d x,48,95 -d y,760,790"
  g=$( echo $( basename $f) | sed -e "s/eORCA025/GB/g" )
  ncks -O  $zoom $f  $g

 # IW  Iriwadi
  echo  ... IW 
  zoom=" -d x,77,104 -d y,739,760"
  g=$( echo $( basename $f) | sed -e "s/eORCA025/IW/g" )
  ncks -O  $zoom $f  $g
done

   

