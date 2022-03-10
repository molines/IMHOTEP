# eORCA025.L75-IMHOTEP.SC configuration

## Description :
  This configuration is a clone of the eORCA025.L75-IMHOTEP.S configuration. The only difference consists in using 
climatological JRA55 forcing (and climatological precip correction term as well)

 This simulation will be use for detrending interannual simulations performed in WP1, over the period 1980-2018. 
Therefore, this simulation will use the same restart file (form spinup run 01/01/1980) as the WP1 simulations. The
JRA55 climatology will correspond to the 1980-2018 climatology.

## Building JRA55 climatology:
Note that when forcing a climatological run, U10 V10 are replaced by WU10, WV10 and W10 climatology. W being the wind 
module. 
  * 3-hourly climatology.

  * smoothing ? 

## Building SSS correction term climatology.
  * period 1980-2018 
