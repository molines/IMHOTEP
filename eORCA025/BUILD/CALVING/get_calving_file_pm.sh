#!/bin/bash
CONFIG=eORCA025.L75
CONFIG2D=${CONFIG%.*}

OPENDAP=https://ige-meom-opendap.univ-grenoble-alpes.fr/thredds/fileServer/meomopendap/extract/eORCA025.L121/eORCA025.L121-I/
CALVING=eORCA025_calving_b0.2_v2.3.nc

# Get Pierre Mathiot file for OPENDAP if necessary
if [ ! -f $CALVING ] ; then
  echo "Get original file from MEOM OPENDAP.."
  wget $OPENDAP/$CALVING
fi
# add nav_lon, nav_lat and time_counter variable for compliance with cdftools

echo "Add nav_lon, nav_lat and time_counter variable for compliance with cdftools"
ncks -A -v nav_lon,nav_lat,time_counter ../eORCA025.L75_shlat2d.nc $CALVING
mv $CALVING ztmpfile.nc
# eliminate north hemisphere data (hard coded zoom)
echo "eliminate north hemisphere data "
cdfvar -f ztmpfile.nc -z 1 1442 650 1207 -raz  -v soicbclv
echo "Compress and rename resulting file"
ncks -O -4 -L1 ztmpfile.nc.01 ${CALVING%.nc}_ANTARCTIC.nc

echo "Clean temporary files"
rm  ztmpfile.nc*

