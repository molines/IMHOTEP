#!/bin/bash

CONFIG=eORCA025.L75

cd $WORK/${CONFIG}/${CONFIG}-I/RUNOFF_MASK_DEP-build

ln -sf ../eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc ./
ln -sf  ../eORCA025.L75_y1950-2020_1m_greenland_rnfbis.nc ./
ln -sf ../RUNOFF_ANTARCTIC-build/eORCA025_mskisf_b0.2_c3.0_d1.0_v0.0.nc ./
ln -sf ../eORCA025.L75_y1950-2020_1m_greenland_isfbis.nc ./
ln -sf ../eORCA025_rnfisf_b0.2_c3.0_d1.0_v0.0.nc ./

ln -sf $DEVGIT/IMHOTEP/eORCA025/BUILD/RUNOFF_MASK_DEP/create_rnf_dep_mask.x ./

./create_rnf_dep_mask.x

