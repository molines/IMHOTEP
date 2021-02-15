#!/bin/bash
# data comes from NOAA site at https://www.nodc.noaa.gov/OC5/woa18/

#  m = 00        ==> annual climatology
#  m = 01 --> 12 ==> monthly climatology
#  m = 13-16     ==> quarterly climatology

res=0.25     # can be either 0.25 or 1.00 (degrees)
decad=decav  # indicate the decade to take : decav =1981-2010 ; 5564 = 1955-1964, 6574, 7584, 8594,95A4 (1995-2004), A5B7 (2005-2017)
prefix=woa18_${decad}
URL=https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa

####################################################################

case $res in
(0.25) ext=04 ;;
(1.00) ext=01 ;;
esac

# temperatures (in situ)
URLT=$URL/temperature/${decad}/$res
for m in {00..16} ; do
  wget $URLT/${prefix}_t${m}_${ext}.nc
done

#Salinity (relative)
URLS=$URL/salinity/${decad}/$res
for m in {00..16} ; do
 wget $URLS/${prefix}_s${m}_${ext}.nc
done
