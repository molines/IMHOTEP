#!/bin/bash

CONFIG=eORCA025.L75

cd $WORK/${CONFIG}/${CONFIG}-I/RNF_ISF-build
OPENDAP=https://ige-meom-opendap.univ-grenoble-alpes.fr/thredds/fileServer/meomopendap/extract/eORCA025.L121/eORCA025.L121-BLD/ISF/eORCA025_mskisf_b0.2_c3.0_d1.0_v0.0

MESH_MASK=$WORK/${CONFIG}/${CONFIG}-I/${CONFIG}_mesh_mask_closed_seas_greenland.nc
ln -sf $MESH_MASK mask.nc
ln -sf $MESH_MASK mesh_hgr.nc
ln -sf $MESH_MASK mesh_zgr.nc

ln -sf $DEVGFIT/CDFTOOLS/bin/cdfisf_rnf ./

CONFIG2D=${CONFIG%.*}

MSK_ISF=${CONFIG2D}_mskisf_b0.2_c3.0_d1.0_v0.0.nc
RIGNOT=${CONFIG2D}_mskisf_c3.0_v0.0.txt

# get file if necessary
if [ ! -f $RIGNOT ] ; then
   wget $OPENDAP/$RIGNOT
fi

if [ ! -f $MSK_ISF ] ; then
   wget $OPENDAP/$MSK_ISF
fi

RNF_ISF=${CONFIG2D}_rnfisf_b0.2_c3.0_d1.0_v0.0.nc


./cdfisf_rnf  -w 1 -f $MSK_ISF -v mask_isf \
                   -l $RIGNOT \
                   -b mask.nc  -vb tmaskutil \
                   -i $MSK_ISF -vi mask_isf -nc4 \
                   -o $RNF_ISF
