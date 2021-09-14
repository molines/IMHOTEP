#!/bin/bash

module load ffmpeg
region=PACIF

CONFIG=eORCA025.L75

for typ in S GAI AI GI GA ; do
  CASE=IMHOTEP.$typ
  CONFCASE=${CONFIG}-${CASE}

  cd $WORK/${CONFIG}/${CONFCASE}-PLOT/SAL/$region

  mkmp4_from_png.sh ${CASE}_SAL_$region
done
