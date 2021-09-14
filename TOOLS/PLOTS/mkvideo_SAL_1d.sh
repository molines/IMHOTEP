#!/bin/bash

module load ffmpeg
region=NINDIAN

CONFIG=eORCA025.L75

for typ in S GAI AI GI GA ; do
  CASE=IMHOTEP.$typ
  CONFCASE=${CONFIG}-${CASE}

  cd $WORK/${CONFIG}/${CONFCASE}-PLOT/sosaline/${region}
   for YEAR in {1980..2018} ; do
    /gpfswork/rech/bcn/rcli002/DEVGIT/IMHOTEP/TOOLS/PLOTS/mkmp4_from_png.sh ${CASE}_SSS_${region}_${YEAR} $YEAR
   done
done
