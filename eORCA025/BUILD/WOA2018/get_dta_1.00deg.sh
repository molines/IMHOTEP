#!/bin/bash


URLT=https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/temperature/decav/1.00

# temperatures (in situ)
for m in {00..16} ; do
  wget $URLT/woa18_decav_t${m}_01.nc
done

URLS=https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/salinity/decav/1.00
#Salinity (relative)
for m in {00..16} ; do
 wget $URLS/woa18_decav_s${m}_01.nc
done
