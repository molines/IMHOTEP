#!/bin/bash
# script used to make a montage of several images in a single one
# Note that in this script, it is assumed that thumbnails are organized 3 x 3 (hardcoded)

geometry="603x335"     # pixel size of each thumbnail

CONFIG=eORCA025.L75    # Nemo configuration on which data are interpolated
DTASET=WOA18           # Name of the WOA data set used so far

PERIOD1=81B0           # name of first WOA dataset
PERIOD2=CLIM           # name of second WOA dataset
var=vosaline           # netcdf variable in files
xtra=""                # extra information such as depth, por instance '_1200'


# 1 year
montage -tile 1x3 -geometry $geometry ${CONFIG}_${PERIOD1}_${DTASET}_1y_${var}.png ${CONFIG}_${PERIOD2}_${DTASET}_1y_${var}.png ${CONFIG}_${PERIOD1}-${PERIOD2}_${DTASET}_1y_${var}.png ${DTASET}_${var}${xtra}_1y_${PERIOD1}-${PERIOD2}_comp.png



for season in JFM AMJ JAS OND ; do
cmd="montage -tile 3x3 -geometry $geometry "
case $season in 
(JFM) for m in {01..03} ; do
         cmd="$cmd ${CONFIG}_${PERIOD1}_${DTASET}_1m_${var}_${m}.png "
      done
      for m in {01..03} ; do
         cmd="$cmd ${CONFIG}_${PERIOD2}_${DTASET}_1m_${var}_${m}.png "
      done
      for m in {01..03} ; do
         cmd="$cmd ${CONFIG}_${PERIOD1}-${PERIOD2}_${DTASET}_1m_${var}_${m}.png "
      done
      cmd="$cmd ${DTASET}_${var}${xtra}_${season}_${PERIOD1}-${PERIOD2}_comp.png"
      $cmd ;;
(AMJ) for m in {04..06} ; do
         cmd="$cmd ${CONFIG}_${PERIOD1}_${DTASET}_1m_${var}_${m}.png "
      done
      for m in {04..06} ; do
         cmd="$cmd ${CONFIG}_${PERIOD2}_${DTASET}_1m_${var}_${m}.png "
      done
      for m in {04..06} ; do
         cmd="$cmd ${CONFIG}_${PERIOD1}-${PERIOD2}_${DTASET}_1m_${var}_${m}.png "
      done
      cmd="$cmd ${DTASET}_${var}${xtra}_${season}_${PERIOD1}-${PERIOD2}_comp.png"
      $cmd ;;
(JAS) for m in {07..09} ; do
         cmd="$cmd ${CONFIG}_${PERIOD1}_${DTASET}_1m_${var}_${m}.png "
      done
      for m in {07..09} ; do
         cmd="$cmd ${CONFIG}_${PERIOD2}_${DTASET}_1m_${var}_${m}.png "
      done
      for m in {07..09} ; do
         cmd="$cmd ${CONFIG}_${PERIOD1}-${PERIOD2}_${DTASET}_1m_${var}_${m}.png "
      done
      cmd="$cmd ${DTASET}_${var}${xtra}_${season}_${PERIOD1}-${PERIOD2}_comp.png"
      $cmd ;;
(OND) for m in {10..12} ; do
         cmd="$cmd ${CONFIG}_${PERIOD1}_${DTASET}_1m_${var}_${m}.png "
      done
      for m in {10..12} ; do
         cmd="$cmd ${CONFIG}_${PERIOD2}_${DTASET}_1m_${var}_${m}.png "
      done
      for m in {10..12} ; do
         cmd="$cmd ${CONFIG}_${PERIOD1}-${PERIOD2}_${DTASET}_1m_${var}_${m}.png "
      done
      cmd="$cmd ${DTASET}_${var}${xtra}_${season}_${PERIOD1}-${PERIOD2}_comp.png"
      $cmd ;;
esac

done
