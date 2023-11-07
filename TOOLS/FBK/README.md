# FBK : Feed BacK tool : reformat altimetric data from AVISO to fbk

## Context:
OBS module in NEMO is using input file in FeedBacK format (fbk).  Available data set for altimetry, on the AVISO data server,
are in a different format.

In this directory, we develop a new tool aiming at the conversion AVISO to fbk.

## Structure of the AVISO files for sla:
### Dimensions:
  * One dimension only (time) 
### Variables (time) :
  * double time : (days since 1950-01-01 00:00:00)
  * int longitude (add_offset = 0, scale_factor = 1.e-6)
  * int latitude (add_offset = 0, scale_factor = 1.e-6)
  * short cycle : "Cycle the measurement belongs to"
  * short track : "Track in cycle the measurement belongs to"
  * short sla_unfiltered : "Sea level anomaly not-filtered not-subsampled with dac, ocean_tide and lwe correction applied"  
     add_offset = 0, scale factor = 0.001
  * short sla_filtered : "Sea level anomaly filtered not-subsampled with dac, ocean_tide and lwe correction applied"  
     add_offset = 0, scale factor = 0.001
  * short dac : "Dynamic Atmospheric Correction"  
     add_offset = 0, scale factor = 0.001
  * short ocean_tide:  "Ocean tide model"  
     add_offset = 0, scale factor = 0.001
  * short internal_tide : "Internal tide correction"  
     add_offset = 0, scale factor = 0.001
  * short lwe : "Long wavelength error"  
     add_offset = 0, scale factor = 0.001
  * short mdt : "Mean dynamic topography"  
     add_offset = 0, scale factor = 0.001
### Scalar 
  * short tpa_correction: "TOPEX-A instrumental drift correction derived from altimetry and tide gauges global comparisons (WCRP Sea Level Budget Group, 2018)"  
     add_offset = 0, scale factor = 0.001

## Structure of the FBK file 
### Dimensions:
  * N_OBS = 9000289 
  * N_LEVELS = 1 
  * N_VARS = 1 
  * N_QCF = 2 
  * STRINGNAM = 8 
  * STRINGWMO = 8 
  * STRINGTYP = 4 
  * STRINGJULD = 14 
### Variables 
  * double LONGITUDE(N_OBS)  : eg 49.548605
  * double LATITUDE(N_OBS)  :  eg -44.59194
  * char VARIABLES(N_VARS, STRINGNAM) : 'SLA'
  * char STATION_IDENTIFIER(N_OBS, STRINGWMO): 'J1'
  * char STATION_TYPE(N_OBS, STRINGTYP) : ' 7'
  * double DEPTH(N_OBS, N_LEVELS)  : 0
  * int DEPTH_QC(N_OBS, N_LEVELS)  : fill
  * int DEPTH_QC_FLAGS(N_OBS, N_LEVELS, N_QCF)  O -99999
  * double JULD(N_OBS) : eg : 22645.0000179
  * char JULD_REFERENCE(STRINGJULD) : '19500101000000'
  * int OBSERVATION_QC(N_OBS) fill
  * int OBSERVATION_QC_FLAGS(N_OBS, N_QCF)  fill fill
  * int POSITION_QC(N_OBS)   fill
  * int POSITION_QC_FLAGS(N_OBS, N_QCF)  fill fill
  * int JULD_QC(N_OBS) fill
  * int JULD_QC_FLAGS(N_OBS, N_QCF) fill fill
  * int ORIGINAL_FILE_INDEX(N_OBS) fill
  * float SLA_OBS(N_OBS, N_LEVELS) value in m ( error in PA file)
  * int SLA_QC(N_OBS) 1
  * int SLA_QC_FLAGS(N_OBS, N_QCF) fill fill
  * int SLA_LEVEL_QC(N_OBS, N_LEVELS) 1
  * int SLA_LEVEL_QC_FLAGS(N_OBS, N_LEVELS, N_QCF) fill fill

All the QC and QC flags are not used in NEMO ...!


## Correspondance between dt_global_j2_phy_l3_20120101_20210603.nc and fdbk_j2_2012.nc
First time in fdbk_j2_2012.nc is 22645.0000179  
time(1024) is 22645.0000179 in dt_global_j2_phy_l3_20120101_20210603.nc  

## Convertion program
[aviso_fdbk][./avisou_fdbk.f90] program has been written to perform the reformating of AVISO/CMEMS files  to feedback format:

```
 USAGE : aviso_fdbk -f AVISO-file -sat SAT-type
     Purpose:
        Convert the input SLA aviso file into fdbk format
     Options:
        -f AVISO-file : give the name of SLA aviso file
        -sat SAT-type : a 4 character max variable giving
              a code for the satelite. e.g: tp, j1,j2,j3...
              This follows the CMEMS nomenclature.
     Output:
        Output filename will be 'fdbk_'<AVISO-file>
     References :
        https://marine.copernicus.eu/ for original aviso
        data set

```

A bash script ([aviso2fdbk.sh](./aviso2fdbk.sh) ) was setup on cal1 for performing the reformating.    
AVISO/CMEMS files are daily files. For OBS in NEMO, as far as we are able to perform monthly segments, gathering daily files in 
monthly files seems adequate.   
[concat_month.sh](./concat_month.sh) script perform this gathering after the files are reformatted to feed-back.

## Data set :
  * On **ige-meom-cal1.u-ga.fr** Feed Back files are archived on /mnt/meom/DATA_SET/AVISO-1993-2020-fdbk/ 
  * on **jean-zay.idris.fr** Feed Back Files are available on /gpfswork/rech/cli/commun/DATA_SET/OBS/AVISO and archived also on
/gpfsstore/rech/cli/commun/DATASET/OBS/AVISO.  
For simulation production, links have been made in /gpfswork/rech/cli/commun/DATA_SET/OBS/AVISO/ALL
([mkall_links.sh](./mkall_links.sh) ) with all sla file spanning the
period 1993-2020.  Generic name such as `fdbk_dt_global_SAT_phy_l3_202010.nc`  was used (changing the name of the satelite to SAT).
Netcdf variable `STATION_IDENTIFIER` gives the name of the satelite (in this example 'j3' for jason3.






