# Building the runoff file

## Getting the data:
After a concerted discussion within the IMOTHEP group, and after evaluation of the available data set for river runoff, 
decision was taken to use the ISBA reanalysis for years 1979-2019,  with daily values.

William Llowel made the original data file available on file_sender. The file was retrieved and copied on our cal1 server:

   `/mnt/meom/DATA_SET/RUNOFF_ISBA/sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019.nc `

Worth to be noted that the original files weights more than 28 Gb because it uses netcdf3 file format.  Converting to netcdf4 with deflation level of 1 reduces
the file to 1.6 Gb 

## Description of the data:
The file provides the river discharge (`rivdis`) on a regular 1/2 degree geographic grid.  Unit for the river discharge is `m3/sec`. Therefore,
the data mesh have  720x360 grid points, and data are available on continental coast line. Out of the coastline, data set takes the `_FillValue`
 value (1.0e20).  Longitude (netcdf variable `longitude`) span the range -179.5 to 179.5 degrees. Latitude (netcdf variable `latitude` span the
range -89.5 to 89.5 degrees. Time counter is using units in days since 1979-01-01 00:00. The gregorian calendar is used.

## Processing the data:
In the IMOTHEP project the very first runs are foreseen as forced by climatological runoff. Therefore, the first task is to compute this
climatological runoff (daily).  As the original file syteme uses a greogorian calendar, with leap years, we adopt the following processing :

Prepare a file without feb 29, using cdo

   ``` 
    cdo -z zip_1 del29feb sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019.nc sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019_noleap.nc
   ```

Then we prepare the daily climatology, still with nco:

   ``` 
    cdo -z zip_1 ydaymean    sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019_noleap.nc sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_ydaymean.nc
   ``` 

## Preparing the NEMO runoff data file.
### using Julien Jouanno python script as a model.
Julien kindly gives us the [python script](./build_runoff_fromISBA.py) he set up for creating NEMO runoff file from ISBA reanalysis. We
used it after adaptation to our global domain.
