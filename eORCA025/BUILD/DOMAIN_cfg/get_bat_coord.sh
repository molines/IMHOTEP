#!/bin/bash
# this script get the bathymetry and coordinates file form the MEOM opendap.
# Those files are used in the preparation of the domain_cfg file

URL=https://ige-meom-opendap.univ-grenoble-alpes.fr/thredds/fileServer/meomopendap/extract/eORCA025.L121/eORCA025.L121-I

#wget  --no-check-certificate $URL/eORCA025_bathymetry_b0.2.nc
wget  --no-check-certificate $URL/eORCA025_coord_c3.0.nc 

# make the link for make_domain_cfg.exe
ln -sf eORCA025_bathymetry_b0.2.nc bathy_meter.nc
ln -sf eORCA025_coord_c3.0.nc coordinates.nc
