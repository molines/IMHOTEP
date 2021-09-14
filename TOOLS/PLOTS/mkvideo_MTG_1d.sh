#!/bin/bash
#set -x 

module load ffmpeg
region=NINDIAN

CONFIG=eORCA025.L75

  CASE=IMHOTEP.S-GAI
  CONFCASE=${CONFIG}-${CASE}

#  cd $WORK/${CONFIG}/${CONFCASE}-PLOT/sosaline/${region}
  cd   $DDIR/zmontage_eORCA025.L75_IMHOTEP.S-IMHOTEP.GAI_sosaline_ATLTROP
   for YEAR in {1981..2006} ; do
    /gpfswork/rech/bcn/rcli002/DEVGIT/IMHOTEP/TOOLS/PLOTS/mkmp4_from_png.sh ${CASE}_SSS_${region}_${YEAR} $YEAR
   done
