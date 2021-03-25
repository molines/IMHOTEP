#!/bin/bash

# This script compute the distance to coast file from runoff file

CF_MESH_HGR=../eORCA05.L75_domain_cfg.nc
CF_MESH_MASK=../eORCA05.L75_mesh_mask.nc
CF_RUNOFF=../eORCA05_runoff_ISBA_1979_3.5.nc
CF_3D_TFILE=../eORCA05.L75_81B0_WOA18_1m_vosaline.nc

CF_DIST_COAST=eORCA05.L75_distcoast_v4.nc

# Compute dist.coast file from the runoff file
 cdfcofdis  -H $CF_MESH_MASK  -M $CF_RUNOFF  -T $CF_3D_TFILE -rnf  -nc4

 # Medsea East + Black Sea + Red Sea     577  663  404 459
 cdfbathy -f  dist.coast -v Tcoast -zoom 577  663  404 459   -sz 10000000
 # Medsea West (Alboran sea)               564 577 413 439
 cdfbathy -f dist.coast.01 -v Tcoast -zoom 564 577 413 439 -sz 10000000
 # mask the resulting file (restoring islands, but not a pb)
 cdfmltmask -f  dist.coast.02 -m $CF_MESH_MASK  -v Tcoast -p T
 mv  dist.coast.02_masked $CF_DIST_COAST

# extra cleaning of temporary files 
rm dist.coast.01 dist.coast.02

