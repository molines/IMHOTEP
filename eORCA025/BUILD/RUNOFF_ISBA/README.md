# Building the river runoff file from ISBA land system

## 1. Getting the data:
After a concerted discussion within the IMHOTEP group, and after evaluation of the available data set for river runoff, 
decision was taken to use the ISBA reanalysis for years 1979-2019,  with daily values.

William Llowel made the original data file available on file_sender. The file was retrieved and copied on our cal1 server:

   `/mnt/meom/DATA_SET/RUNOFF_ISBA/sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019.nc `

Worth to be noted that the original files weights more than 28 Gb because it uses netcdf3 file format.  Converting to netcdf4 with deflation level of 1 reduces
the file to 1.6 Gb 

## 2. Description of the data:
The file provides the river discharge (`rivdis`) on a regular 1/2 degree geographic grid.  Unit for the river discharge is `m3/sec`. Therefore,
the data mesh have  720x360 grid points, and data are available on continental coast line. Out of the coastline, data set takes the `_FillValue`
 value (1.0e20).  Longitude (netcdf variable `longitude`) span the range -179.5 to 179.5 degrees. Latitude (netcdf variable `latitude` span the
range -89.5 to 89.5 degrees. Time counter is using units in days since 1979-01-01 00:00. The gregorian calendar is used.

## 3. Processing the data:
  * Creating a climatology of input data
In the IMHOTEP project the very first runs are foreseen as forced by climatological runoff. Therefore, the first task is to compute this
climatological runoff (daily).  As the original file syteme uses a gregorian calendar, with leap years, we adopt the following processing :

Prepare a file without feb 29, using cdo

  ``` 
    cdo -z zip_1 del29feb sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019.nc sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019_noleap.nc
  ```

Then we prepare the daily climatology, still with nco:

  ``` 
    cdo -z zip_1 ydaymean    sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019_noleap.nc sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_ydaymean.nc
  ``` 

## 4. Preparing the NEMO runoff data file.
### 4.1 Projection of ISBA climatology on the coastal points of eORCA025
Julien Jouanno kindly gives us the [python script](./build_runoff_fromISBA.py) he set up for creating NEMO runoff file from ISBA reanalysis. We
used it after adaptation to our global domain. See the [modified script](./build_ORCA025_runoff_fromISBA.py),  with the following modifications: 
  * adapt file names
  * change the search radius  from 0.75 degree to 2.5 after some tests
  * add diagnostics on the total river discharge used on the NEMO grid
  * add diagnostics on the ISBA river discharge not taken into account in NEMO (too far from NEMO littoral points).
  * add one file output (on ISBA grid) showing the points not taken into account (for check). Values of this file correspond to the climatological mean of the river discharge. Looking at this map, we see that missing points are essentially located along Antarctica and around closed seas or lakes (e.g. Caspian sea, Great US lake etc... ), which is very acceptable.

### 4.2 Projection of ISBA interannual dataset  on the coastal points of eORCA025
We use a variant of the former python script where the only real change is the loop over years 1979-2018, and the name of the input ISBA file. See 
[modified python script](./build_ORCA025_runoff_inerannual_fromISBA.py) .
  * This script produce a set of yearly files, with daily runoff, in suitable units for NEMO (kg/m2/s).

### 4.3 Verification and modifications.
After this step, we have the first guess file for runoff. This file still requires some adjustment for use in the project:
  * Modifications along Antartica and Greenland: bash script [reset_AA_GR_to_0.sh](./reset_AA_GR_to_0.sh) was written for this purpose, based on cdfvar tool. In this script
runoff values around Antarctica and Greenland are reset to 0, but the runoff mask (socoefr0) remains unchanged, as somehow ther will be runoff on these points, from other sources.
  * Note that in the actual code, runoff points corresponding to mean annual discharge greater than 2000 m3/s are spread on the 3 nearest points, instead of one. This differs from 
what we used to do in previous DRAKKAR runs (using Dai and Trenberth runoff dataset), where rivermouths  were widely spread. In IMHOTEP, we use a non-linear free surface, and
runoff are really taken as a freshwater flux (and not a virtual salt flux). We are quite confident that doing so, the freshwater flux on a grid point, can be much higher than in the
case of virtual salt flux (with negative salinity issues). Nevertheless, we keep in mind the possibilily to spread rivermouths on larger areas, if problems arise.
