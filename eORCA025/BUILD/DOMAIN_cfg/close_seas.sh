#!/bin/bash
# assuming CDFTOOLS are available
# this scipts fills the closed seas in eORCA025 bathymetric file
# index are hard coded !

# work on a copy
cp bathy_meter.nc tmp_bathy.nc

# fill in Great Lakes (USA)    Bathymetry
cdfbathy -f tmp_bathy.nc  -o -var Bathymetry -zoom 780 850 869 929 -raz

# fill in Lake Victoria (Africa)
cdfbathy -f tmp_bathy.nc -o -var Bathymetry -zoom 1272 1292 667 690 -raz

# fill in Caspian and Aral seas
cdfbathy -f tmp_bathy.nc -o -var Bathymetry -zoom 1310 1404 841 964 -raz

# fill in Azov Sea
cdfbathy -f tmp_bathy.nc -o -var Bathymetry -zoom 1284 1303 908 931 -raz

# fill in Lake Maracaibo (Venezuela)
cdfbathy -f tmp_bathy.nc -o -var Bathymetry -zoom 859 867 718 729 -raz

# rename tmp file to final name
mv tmp_bathy.nc eORCA025_bathymetry_b0.2_closed_seas.nc

# update bathy_meter.nc link previous to make_domain_cfg.exe
rm bathy_meter.nc
ln -sf eORCA025_bathymetry_b0.2_closed_seas.nc bathy_meter.nc
