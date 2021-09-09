#!/bin/bash

module load ffmpeg

CONFIG=eORCA025.L75

for typ in S GAI AI GI GA ; do
  CASE=IMHOTEP.$typ
  CONFCASE=${CONFIG}-${CASE}

  cd $WORK/${CONFIG}/${CONFCASE}-PLOT/TEMPERATURE/ATLIND

  mkmp4_from_png.sh ${CASE}_CT_ATLIND
done
