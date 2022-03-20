# eORCA025.L75-IMHOTEP.SC configuration
## 1. Overview
This configuration is identical to IMHOTEP.S  in term of code and parameterization. The main difference is
that it uses a JRA55 daily climatology (1980-2019) as atmospheric forcing.  The goal of this climatological
run is primarily to help for detrending interannual run performed in IMHOTEP WP1. The idea is to use a 
similar procedure than in OCCIPUT for retrieving physical local trends in various fields, but primarily for 
the SSH.  Therefore, this simulation will use the same restart file (form spinup run 01/01/1980) as the WP1 
simulations.

As for IMHOTEP.S, all continental fresh water forcing are climatological. However, we note an extra difference
concerning the fresh water flux correction term (replacing SSS restoring): in IMHOTHEP.S, this correction
term was kept as monthly interannual, in IMHOTEP.SC it is a monthly climatology over 1980-2019.

## 2. Preparing Climatological forcing
After some internal discutions and tests, we decided to follow the standard procedure for preparing the 
climatological forcing field. Tools were adapted for IMHOTEP from JMMTOOLS/FORCING_TOOLS/CLIMATO and are
availbale in this [directory](../TOOLS/CLIMATOLOGICAL_FORCING/src)
The goal is to obtain a 365 days annual climatology suitable for use in 
IMHOTEP.SC. A 10 days smoothing filter is used at the end to get read of unphysical noise in the local 
time series of the climatology. We performed the following steps:

### 2.1 Computing wind speed and pseudo stress (3-hourly)
When forcing with an atmospheric climatology, it is important that  overall momemtum fluxes are comparable
with overall momentum fluxes used with the interannual forcing.  Wind stress is proportional to a product of a
wind speed by either zonal or meridional wind component at 10m (pseudo stress). In order to conserve momemtum 
fluxes, we compute the climatology of the non linear term : W10, W10 x U10 and W10 x V10 (W10 being the 
wind speed). NEMO code (DCM version) is modified to read these new fields instead of U10 and V10.
  * [mk_W10_WU10_WV10.sh](../TOOLS/CLIMATOLOGICAL_FORCING/mk_W10_WU10_WV10.sh) :  compute the 3-hourly windspeed
files, and the 3-hourly pseudo stress files.

### 2.2 Get read of February 29 on leap years:
  * [remove_feb29.sh](../TOOLS/CLIMATOLOGICAL_FORCING/remove_feb29.sh) : This script is based on cdo which is
clever enough to handle this task properly !

### 2.x Preparing WDMP climatology.

## 3. Setting up the namelist for this run
  * Lionel Renault Current Feed Back parametrisation
  * Leap or no leap ?
  * change in the input files
