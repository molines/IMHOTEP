#!/bin/bash
geometry="603x335"

PERIOD1=81B0
PERIOD2=CLIM
var=vosaline
xtra=""


# 1 year
montage -tile 1x3 -geometry $geometry eORCA025.L75_${PERIOD1}_WOA18_1y_${var}.png eORCA025.L75_${PERIOD2}_WOA18_1y_${var}.png eORCA025.L75_${PERIOD1}-${PERIOD2}_WOA18_1y_${var}.png WOA18_${var}${xtra}_1y_${PERIOD1}-${PERIOD2}_comp.png



for season in JFM AMJ JAS OND ; do
cmd="montage -tile 3x3 -geometry $geometry "
case $season in 
(JFM) for m in {01..03} ; do
         cmd="$cmd eORCA025.L75_${PERIOD1}_WOA18_1m_${var}_${m}.png "
      done
      for m in {01..03} ; do
         cmd="$cmd eORCA025.L75_${PERIOD2}_WOA18_1m_${var}_${m}.png "
      done
      for m in {01..03} ; do
         cmd="$cmd eORCA025.L75_${PERIOD1}-${PERIOD2}_WOA18_1m_${var}_${m}.png "
      done
      cmd="$cmd WOA18_${var}${xtra}_${season}_${PERIOD1}-${PERIOD2}_comp.png"
      $cmd ;;
(AMJ) for m in {04..06} ; do
         cmd="$cmd eORCA025.L75_${PERIOD1}_WOA18_1m_${var}_${m}.png "
      done
      for m in {04..06} ; do
         cmd="$cmd eORCA025.L75_${PERIOD2}_WOA18_1m_${var}_${m}.png "
      done
      for m in {04..06} ; do
         cmd="$cmd eORCA025.L75_${PERIOD1}-${PERIOD2}_WOA18_1m_${var}_${m}.png "
      done
      cmd="$cmd WOA18_${var}${xtra}_${season}_${PERIOD1}-${PERIOD2}_comp.png"
      $cmd ;;
(JAS) for m in {07..09} ; do
         cmd="$cmd eORCA025.L75_${PERIOD1}_WOA18_1m_${var}_${m}.png "
      done
      for m in {07..09} ; do
         cmd="$cmd eORCA025.L75_${PERIOD2}_WOA18_1m_${var}_${m}.png "
      done
      for m in {07..09} ; do
         cmd="$cmd eORCA025.L75_${PERIOD1}-${PERIOD2}_WOA18_1m_${var}_${m}.png "
      done
      cmd="$cmd WOA18_${var}${xtra}_${season}_${PERIOD1}-${PERIOD2}_comp.png"
      $cmd ;;
(OND) for m in {10..12} ; do
         cmd="$cmd eORCA025.L75_${PERIOD1}_WOA18_1m_${var}_${m}.png "
      done
      for m in {10..12} ; do
         cmd="$cmd eORCA025.L75_${PERIOD2}_WOA18_1m_${var}_${m}.png "
      done
      for m in {10..12} ; do
         cmd="$cmd eORCA025.L75_${PERIOD1}-${PERIOD2}_WOA18_1m_${var}_${m}.png "
      done
      cmd="$cmd WOA18_${var}${xtra}_${season}_${PERIOD1}-${PERIOD2}_comp.png"
      $cmd ;;
esac

done
