#!/bin/bash
#set -x 

module load ffmpeg
region=ATLIND

CONFIG=eORCA025.L75

  CASE=IMHOTEP.GAI-GAIa
  CONFCASE=${CONFIG}-${CASE}

#  cd $WORK/${CONFIG}/${CONFCASE}-PLOT/sosaline/${region}
  cd   $DDIR/zmontage_eORCA025.L75_IMHOTEP.GAI-IMHOTEP.GAIa_sosaline_ATLIND
   for YEAR in {1997..2007} ; do
    /gpfswork/rech/bcn/rcli002/DEVGIT/IMHOTEP/TOOLS/PLOTS/mkmp4_from_png.sh ${CASE}_SSS_${region}_${YEAR} $YEAR
   done
