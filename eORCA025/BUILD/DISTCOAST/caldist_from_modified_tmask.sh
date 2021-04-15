#!/bin/bash

# This script compute the distance to coast file from runoff file

CF_MESH_HGR=eORCA025.L75_domain_cfg_closed_seas_greenland.nc
CF_MESH_MASK=../eORCA025.L75_mesh_mask.nc
CF_TMASK=eORCA025.L75_tmask_distcoast.nc
CF_3D_TFILE=eORCA025.L75_81B0_WOA18_1y_vosaline.nc

CF_DIST_COAST=eORCA025.L75_distcoast_v2.nc

# Compute dist.coast file from the runoff file
 cdfcofdis  -H $CF_MESH_HGR  -M $CF_TMASK  -T $CF_3D_TFILE  -surf  -nc4

 # Medsea East + Black Sea + Red Sea
 cdfbathy -f  dist.coast -v Tcoast -zoom 1156 1325 732 920   -sz 10000000
 # Medsea West (Alboran sea)
 cdfbathy -f dist.coast.01 -v Tcoast -zoom 1127 1168 831 873 -sz 10000000
 # mask the resulting file (restoring islands, but not a pb)
 cdfmltmask -f  dist.coast.02 -m $CF_MESH_MASK  -v Tcoast -p T
 mv  dist.coast.02_masked $CF_DIST_COAST

# extra cleaning of temporary files 
rm dist.coast.01 dist.coast.02

