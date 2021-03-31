#!/bin/bash

CONFIG=eORCA05.L75

COORD=../${CONFIG}_mesh_mask.nc

# use cdfmkresto build the 2D map of bfr2d coef.
cat << eof > drakkar_bfr.txt
# Construction of bfr2d file for enhanced bottom friction
# Torres strait
# type lon      lat radius rim tau z1 z2
   D    142.5  -10.1  85    15  1   0  0
# Bering strait
# type lon1      lon2   lat1   lat2   rim   tau  z1  z2
   R    -172.3  -166.0  64.7   66.5   1      1   0   0
# Bab El Mandeb
# type lon      lat radius  tau z1 z2
   C    43.4   12.6   30     1  0  0
# Denmark strait
# type lon      lat radius  tau z1 z2
   C   -27.3   65.9   40     1  0   0
eof

cdfmkresto -c $COORD -i drakkar_bfr.txt -o  ${CONFIG}_bfr2d.nc -ov bfr_coef -2d  -val 1 -nc4 


exit

  usage :  cdfmkresto -c COORD-file -i CFG-file [-d DEP-file] [-o DMP-file]...
                      ...[-ov VAR-out] [-2d] [-prev RESTO-file RESTO-var ] ...
                      ...[-val VALUE] [-nc4] [-h]


