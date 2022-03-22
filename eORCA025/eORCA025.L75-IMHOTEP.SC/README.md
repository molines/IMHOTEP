# eORCA025.L75-IMHOTEP.SC configuration
## 1. Overview
This configuration is identical to IMHOTEP.S  in term of code and parameterization. The main difference is
that it uses a JRA55 daily climatology (1980-2019) as atmospheric forcing.  The goal of this climatological
run is primarily to help for detrending interannual run performed in IMHOTEP WP1. The idea is to use a 
similar procedure than in OCCIPUT for retrieving physical local trends in various fields, but primarily for 
the SSH.  Therefore, this simulation will use the same restart file (from spinup run 01/01/1980) as the WP1 
simulations.

As for IMHOTEP.S, all continental fresh water forcing are climatological. However, we note an extra difference
concerning the fresh water flux correction term (replacing SSS restoring): in IMHOTHEP.S, this correction
term was kept as monthly interannual, in IMHOTEP.SC it is a monthly climatology over 1980-2019.

## 2. Preparing Climatological forcing
After some internal discussions and tests, we decided to follow the standard procedure for preparing the 
climatological forcing field. Tools were adapted for IMHOTEP from JMMTOOLS/FORCING_TOOLS/CLIMATO and are
available in this [directory](../../TOOLS/CLIMATOLOGICAL_FORCING/src).
The goal is to obtain a 365 days annual climatology suitable for use in 
IMHOTEP.SC. A 10 days smoothing filter is used at the end to get rid of unphysical noise in the local 
time series of the climatology. We performed the following steps:

### 2.1 Computing wind speed and pseudo stress (3-hourly)
When forcing with an atmospheric climatology, it is important that  overall momemtum fluxes are comparable
with overall momentum fluxes used with the interannual forcing.  Wind stress is proportional to a product of a
wind speed by either zonal or meridional wind component at 10m (pseudo stress). In order to conserve momemtum 
fluxes, we compute the climatology of the non linear term : W10, W10 x U10 and W10 x V10 (W10 being the 
wind speed). NEMO code (DCM version) is modified to read these new fields instead of U10 and V10.
  * [mk_W10_WU10_WV10.sh](../../TOOLS/CLIMATOLOGICAL_FORCING/mk_W10_WU10_WV10.sh) :  compute the 3-hourly windspeed
files, and the 3-hourly pseudo stress files.

### 2.2 Get rid of February 29 on leap years:
  * [remove_feb29.sh](../../TOOLS/CLIMATOLOGICAL_FORCING/remove_feb29.sh) : This script is based on cdo which is
clever enough to handle this task properly ! Note that at the end of this step, all files in the TMP_CLIMATO
working directory are **noleap** files although they have the same name as original files !

### 2.3 Compute daily climatology from noleap 3-hourly files
  * [mkclimato.sh](../../TOOLS/CLIMATOLOGICAL_FORCING/mkclimato.sh) computes the daily climatology from the **noleap**
files in TMP_CLIMATO. It ends up with climatological files in the JRA55/CLIMATO_1980-2019
directory.  Note that both unfiltered (*eg* drowned_tas_JRA55_CLIM_1980-2019.nc) and 10-days filtered files
(*eg* drowned_tas_JRA55_CLIM_1980-2019-f.nc)  are available in the climatology. It is likely that we will
use the filtered files.

### 2.4 Preparing WDMP climatology.
WDMP files are buildt from the output of the spinup run, from which we compute an interannual monthly mean on the
eORCA025 model grid.  But they are considered as part of the forcing and although they are not provided
by JRA55 they are somehow linked to this data set (changing the atmospheric forcing will likely change the
SSS restoring term). Note that SSS restoring was faded out in  coastal areas so that there are no
direct impact on the continental fresh water fluxes.  For the climatological run we just compute the monthly
mean over the 1980-2019 period.
  * [mk_WDMP_climato.sh](../../TOOLS/CLIMATOLOGICAL_FORCING/mk_WDMP_climato.sh) is used for this purpose. It is a
light interface to the `cdfmoyt` tools from [CDFTOOLS](https:github.com/meom-group/CDFTOOLS.git)

## 3. Installing the NEMO configuration
For reference we list the procedure for installing the NEMO configuration for this simulation.
### 3.1 Check for valid version of DCM:
In order to be similar with WP1 IMHOTEP run, we need to use exactly the same commit (check install history in 
the IMHOTEP.S config directory:

  ```
   Thu Jul 29 06:19:01 CEST 2021
   origin	https://github.com/meom-group/DCM.git (fetch)
   origin	https://github.com/meom-group/DCM.git (push)
   commit 667c0038ba5fc22e4eebc9b8073d655bed32872d
   Author: jmm <Jean-Marc.Molines@univ-grenoble-alpes.fr>
   Date:   Wed Jul 28 09:48:36 2021 +0200
  ```


## 4. Setting up the namelist for this run
  * Lionel Renault Current Feed Back parametrisation
  * Leap or no leap ?
  * change in the input files
