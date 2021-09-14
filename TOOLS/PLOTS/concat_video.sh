#!/bin/bash

CONFIG=eORCA025.L75
region=ATLTROP

for t in S GAI AI GA GI ; do
  CONFCASE=${CONFIG}-IMHOTEP.$t

  cd  $WORK/${CONFIG}/${CONFCASE}-PLOT/sosaline/${region}
  rm zlst
  for f in movie*.mp4 ; do
     echo file $f >> zlst
  done

  ffmpeg -f concat -safe 0 -i zlst -c copy ${CONFCASE}_y1980-2018_1d_SSS_$region.mp4

done
