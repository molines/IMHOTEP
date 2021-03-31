#!/bin/bash

CONFIG=eORCA05.L75


ln -sf ../${CONFIG}_mesh_mask.nc mask.nc
ln -sf ../${CONFIG}_mesh_mask.nc mesh_hgr.nc
ln -sf ../${CONFIG}_mesh_mask.nc mesh_zgr.nc

# use cdfmkresto build the 2D map of shlat2s
cat << eof > drakkar_shlat2d.txt
# Construction of shlat2d 
# Berring strait
# type lon1      lon2   lat1   lat2   rim   tau  z1  z2
   R    -172.3  -166.0  64.7   66.5   0      1   0   0
# Med Sea
# type lon1  lon2    lat1   lat2   rim   tau  z1  z2
   R   -5.7  42.7    29.5   42.9    0    1    0    0
   R    0.0  42.7    41.8   48.2    0    1    0    0
# West Greenland coast
# type lon   lat radius   rim   tau  z1  z2
   D   -48.9 61.5   100   30    1     0  0
   D   -49.8 62.1   100   30    1     0  0
   D   -50.3 62.7   100   30    1     0  0
   D   -51.2 63.4   100   30    1     0  0
   D   -51.8 64.0   100   30    1     0  0
eof

cdfmkresto -c mesh_hgr.nc -i drakkar_shlat2d.txt -o  ${CONFIG}_shlat2d.nc -ov shlat2d -2d  -val 2 -nc4 


 cdfmltmask  -f ${CONFIG}_shlat2d.nc -m mask.nc -v shlat2d -p T -noup


exit

  usage :  cdfmkresto -c COORD-file -i CFG-file [-d DEP-file] [-o DMP-file]...
                      ...[-ov VAR-out] [-2d] [-prev RESTO-file RESTO-var ] ...
                      ...[-val VALUE] [-nc4] [-h]


