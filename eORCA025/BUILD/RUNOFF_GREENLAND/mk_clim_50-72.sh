#!/bin/bash

CONFIG=eORCA025.L75
y1=1950
y2=1972
freq=1m

cd  $WORK/${CONFIG}/${CONFIG}-I/RUNOFF_GREENLAND-build/ANNUAL_BIS

for typ in calvingbis isfbis rnfbis ; do
  lst=''
  for y in $( seq $y1 $y2 ) ; do
   lst="$lst ${CONFIG}_${freq}_greenland_${typ}_y${y}.nc"
  done
  cdfmoyt -l $lst -nc4 -o  ${CONFIG}_${freq}_greenland_${typ}_clim_y${y1}-${y2}
done


