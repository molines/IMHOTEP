#!/bin/bash

mkdir -p ANNUAL_BIS
for fil in eORCA025.L75_y1950-2020_1m_greenland_calvingbis.nc eORCA025.L75_y1950-2020_1m_greenland_isfbis.nc eORCA025.L75_y1950-2020_1m_greenland_rnfbis.nc ; do
  filbas=${fil%.nc}
  filbas=$(echo $filbas | sed -e 's/_y1950-2020//' )

  m1=1
  for y in {1950..2020} ; do
    m2=$(( m1 + 11 ))
    echo extracting ${filbas}_y${y}.nc $m1  $m2 
    ncks -F -d time_counter,$m1,$m2 $fil ANNUAL_BIS/${filbas}_y${y}.nc
    m1=$(( m2 + 1 ))
  done
done
