#!/bin/bash

mkdir ANNUAL
for fil in eORCA025.L75_1m_greenland_calving.nc eORCA025.L75_1m_greenland_isf.nc eORCA025.L75_1m_greenland_rnf.nc ; do
  filbas=${fil%.nc}

  m1=1
  for y in {1950..2020} ; do
    m2=$(( m1 + 11 ))
    echo extracting ${filbas}_y${y}.nc $m1  $m2 
    ncks -F -d time_counter,$m1,$m2 $fil ANNUAL/${filbas}_y${y}.nc
    m1=$(( m2 + 1 ))
  done
done

