# Improved Bathymetry around Greenland


## 1. Context:
In IMHOTEP, the freshwater flux coming from Greenland is one of the topic to be treated, in order to
estimate the impact of the variation of this flux on the ocean circulation and sea level.

The Greenland freshwater flux (GFWF) has a liquid (melted glacial waters, basal melting) and a solid (calving) contribution. Jeremie Mouginot, Pierre
Mathiot and Nicolas Jourdain provided a file with 262 points around Greenland (corresponding to eORCA025 model cells), with the monthly liquid 
and solid discharges.  In addition, a depth corresponding to the base of the glacier or the the depth of bathymetric sills in the fjords, is given 
and will be used in NEMO to apply the runoff at depth.

For setting up the GFWF file, a very detailed cartography (150m resolution) of Greenland, giving the elevation of the bed-rock, the thickness 
of the ice etc... This is the BedmachineGreenland data base.  Jeremie used the last version (2020.04.10), not yet published of this file. On 
the other hand, the eORCA025.L75 NEMO configuration has already a bathymetric file.  Comparing the runoff depths given in the GFWF data-set 
(infered from BedMachine) and the actual eORCA025.L75 bathymetry, large discrepancies were encountered, whith 127 model points having a depth 
much smaller than the runoff depth. For coherancy, we decided to review the model bathymetry around Greenland, using the BedMachineGreenland-2020-04-10.nc file.

## 2. Procedure used for building a new improved bathymetry around Greenland
### 2.0  Required software: 
  * [NEMOBAT](https://github.com/molines/NEMOBAT.git) and in particular: 
    * [NSDIC_map_trf.f90](https://github.com/molines/NEMOBAT/blob/master/INPUT_UTILITIES/NSIDC_map_trf.f90), used for transforming stereo graphic polar
coordinates to longitude latitude.
    * [bathyinterp.F90](https://github.com/molines/NEMOBAT/blob/master/INTERP0/batinterp.F90), used to interpolate refined bathymetry on the NEMO grid.
    * [apply_history.f90](https://github.com/molines/NEMOBAT/blob/master/INPUT_UTILITIES/apply_history.f90), used to apply hand corrections made with BMGTOOLS and loggued
into an history file.
    * [apply_patch.f90](https://github.com/molines/NEMOBAT/blob/master/INPUT_UTILITIES/apply_patch.f90), used to merge the regional area into the global file.
    * [mergebat.f90](https://github.com/molines/NEMOBAT/blob/master/INPUT_UTILITIES/mergebat.f90), used to patch the bathymetric file with the regionally
refined one.
  * [BMGTOOLS](https://archimer.ifremer.fr/doc/00195/30646/) in order to perfom hand correction on the Bathymetry.

> Theetten Sebastien, Thiebault Benoit, Dumas Franck, Paul Julien (2014). BMGTools : a community tool to handle model grid and bathymetry. Mercator Ocean - Quarterly Newsletter, (49), 94-98. Open Access version : https://archimer.ifremer.fr/doc/00195/30646/

### 2.1  Getting the BedMachine data.
This dataset can be retrieved from NSDIC web site.  The last official version (version 3) being released in 2017-09-20. As the runoff depth were infered
from a more recent data set, BedMachineGreenland-2020-04-10.nc, provided by the Authors to J. Mouginot (personnal comm.), we also use this unpublished
data set, showing quite large differences in some sectors of Greenland. 

### 2.2 Processing the BedMachine data.
BedMachine data are presented on a regular grid (150m resolution), corresponding to a polar stereographic north projection (35W, 70N). The X, Y corresponding
coordinates were used to compute the longitude and latitude of each grid points (Fortran program NSDIC_map_trf.f90 build for this purpose). We endup with a file
having 2 extra variables : nav_lon and nav_lat (longitude, latitude). (BedMachineGreenland-2020-04-10_lon_lat.nc). The full procedure is:
  
```
   # create a lon_lat.nc file with nav_lon, nav_lat corresponding to X Y in BedMachine file
   NSDIC_map_trf.x -lonlat lon_lat.nc  -xy BedMachineGreenland-2020-04-10.nc -conv 3 -hem N 
   # Append nav_lon,nav_lat to BedMachine file :
   ncks -A -v nav_lon,nav_lat lon_lat.nc BedMachineGreenland-2020-04-10.nc BedMachineGreenland-2020-04-10_lon_lat.nc

```

### 2.3 Using BedMachine data to produce a NEMO Bathymetry.
  * Extract a sub-domain of eORCA025 to include the area covered by BedMachine file:

  ```
    ncks -d y,971,1203 eORCA025.L75_mesh_mask.nc eGREENLAND025.L75_mesh_mask.nc
    ncks -d y,971,1203 eORCA025_bathymetry_b0.2_closed_seas.nc eGREENLAND025.L75_bathymetry
  ```
  
  * Interpolate BedMachine bathymetry on the eGREENLAND025 grid. This is done with batinterp program, and this [namelist](./eGREENLAND025.L75_namelist).
  
  ```
  cat << eof > ./eGREENLAND025.L75_namelist
  &naminterpo
   nn_interp = 1       ! interpolation method : O: Arithmetic Average
                       !                        1: Median average
   nn_perio  = 0       ! NEMO periodic conditions (99 : automatic inference)
   
   ! NEMO bathymetry output file
   cn_fout   = 'eGREENLAND025.L75_bathy_meter_002.nc'   ! name of the output file
   cn_varout = 'Bathymetry'       ! name of output variable
   
   ! NEMO coordinate file
   cn_fgrid  = 'eGREENLAND025.L75_mesh_hgr.nc'      ! name of horizontal grid file
   
   ! External Bathymetric file
   cn_fbatin = 'BedMachineGreenland-2020-04-10_lon_lat.nc'  ! name of external baty file
   ln_regin  = .FALSE.  ! True  : Regular external bathy file
                        ! False :Irregular external bathy file
   cn_varin  = 'bed'    ! name of bathymetry in file
   cn_xdim   = 'x'      ! name of X dimension
   cn_ydim   = 'y'      ! name of Y dimension
   cn_lonv   = 'nav_lon'    ! name of longitude variable
   cn_latv   = 'nav_lat'    ! name of latitude variable
   
   ! change sign  of bathymetry
   ln_sign   = .FALSE.  ! change sign for bathymetry (ISF related)
   /
   eof
   
   batinterp.exe -f ./eGREENLAND025.L75_namelist
  ```

    * Note that we choose nn_interp=1 (Median average) as the interpolation method.
    * This procedure was submmitted via a [batch job](./job_batinterp) because it takes more than 1 hour.
    * The procedure provide `eGREENLAND025.L75_bathy_meter_002.nc` file. This file is just the interpolation of BedMachine elevation data set on the NEMO grid. In particular, it has data everywhere, positive Bathymetry corresponds to land value. So some post processing is required
  *  Changing the sign of the bathymetry

  ```
  ncap2 -s "Bathymetry=float(-1.*Bathymetry)" eGREENLAND025.L75_bathy_meter_002.nc eGREENLAND025.L75_bathy_meter_002.1.nc
  ```

  * Get rid of land values (negative Bathymetry)

  ```
  ncap2 -s "where(Bathymetry < 0 ) Bathymetry=0 ;" eGREENLAND025.L75_bathy_meter_002.1.nc eGREENLAND025.L75_bathy_meter_002.2.nc
  ```
  
  * We notice that the bedrock in central Greenland is below the sea level, but is not covered by the ocean.
  * Masking the Bathymetry.  We choose to maintain the same coastal mask than the original file, in order to be able to use the GFWF file already constructed. In this process
we want to identify grid points (if any) that were in the water in the original bathy file and which are now considered as land point after BedMachine interpolation. To do so, we use a 2 level mask : BedMachine and eORCA025 land points are set to 0 and eORCA025 land points only are set to -10 
  
  ```
  cdfmltmask -f eGREENLAND025.L75_bathy_meter_002.2.nc -m eGREENLAND025.L75_mesh_mask.nc -p T        -v Bathymetry -o eGREENLAND025.L75_bathy_meter_002.3.nc
  cdfmltmask -f eGREENLAND025.L75_bathy_meter_002.3.nc -m eGREENLAND025.L75_mesh_mask.nc -p T -s -10 -v Bathymetry -o eGREENLAND025.L75_bathy_meter_002.4.nc -noup
  ```
  
  * The resulting file for this step is eGREENLAND025.L75_bathy_meter_002.4.nc. Looking at this file with ncview shows coastal white cells corresponding to missing values.
These cells were sea points in the original file and are now land points. Next step will make them sea again !

  * Hand correction in order to drown and correct  too shallow coastal values. This was done  with BMGTOOLS, on eGREENLAND025.L75_bathy_meter_002.5.nc, which is a copy of eGREENLAND025.L75_bathy_meter_002.4.nc. The changes performed on the files, point by points are loggued on this [history file](./eGREENLAND025.L75_bathy_meter_002.5_history).  
  
  ```
  #sample of history file :
  ...
  #@ Modification nr: 128
  2021-04-06 15:38:34: H0 at i=   88 and j=  191 changed from:       -0.0 to:      100.0
  2021-04-06 15:38:34: H0 at i=   89 and j=  192 changed from:       -0.0 to:      100.0
  2021-04-06 15:38:34: H0 at i=   90 and j=  192 changed from:        2.4 to:      100.0
  2021-04-06 15:38:34: H0 at i=   82 and j=  194 changed from:        9.0 to:      100.0
  2021-04-06 15:38:34: H0 at i=   85 and j=  194 changed from:        5.8 to:      100.0
  #@ Modification nr: 129
  2021-04-06 15:39:00: H0 at i=   96 and j=  172 changed from:       78.3 to:       90.0
  2021-04-06 15:39:00: H0 at i=   96 and j=  173 changed from:        1.9 to:       90.0
  2021-04-06 15:39:00: H0 at i=   96 and j=  174 changed from:       74.1 to:       90.0
  2021-04-06 15:39:00: H0 at i=   98 and j=  175 changed from:       -0.0 to:       90.0
  #@ Modification nr: 130
  2021-04-06 15:39:32: H0 at i=   98 and j=  170 changed from:        4.5 to:       50.0
  2021-04-06 15:39:32: H0 at i=   98 and j=  171 changed from:       26.5 to:       50.0
  #@ Modification nr: 131
  2021-04-06 15:39:50: H0 at i=   97 and j=  178 changed from:       -0.0 to:      350.0
  #@ Modification nr: 132
  ...
  ```
  
  * The apply_history program was written to apply the corrections loggued in the history file, so that, transition from 002.4 to 002.5 can be performed with :

```
  apply_history.x -b eGREENLAND025.L75_bathy_meter_002.4.nc -h eGREENLAND025.L75_bathy_meter_002.5_history -o eGREENLAND025.L75_bathy_meter_002.5.nc
```
  
  * Final masking, in order to get rid of the '-10 points'

```
  cdfmltmask -f eGREENLAND025.L75_bathy_meter_002.5.nc -m eGREENLAND025.L75_mesh_mask.nc -p T        -v Bathymetry -o eGREENLAND025.L75_bathy_meter_002.6.nc
```
   
  * Merging the bathymetric patch into the eGREENLAND025.L75 bathymetry.  This process perform a weighted average between the original bathymetry and the corrected 
bathymetry that was produced in the former step.
    * This merge process requires a file with the distance to the boundaries for the patch (ie the file coming from 
BedMachine interpolation). This 'distance' file can be produced by cdfcofdis, using a mask file infered from the interpolated bathymetry file, where the values
are arbitrarily set to 9999.99 outside the BedMachine boundaries:
    
    ```
    ncap2 -s "where(Bathymetry >= 9999.9) Bathymetry=0 ; elsewhere  Bathymetry=1. ;" eGREENLAND025.L75_bathy_meter_002.1.nc eGREENLAND025.L75_maskbound.nc
    ncrename -v Bathymetry,tmask eGREENLAND025.L75_maskbound.nc
    cdfcofdis -H eGREENLAND025.L75_mesh_mask.nc  -M eGREENLAND025.L75_maskbound.nc -surf -o eGREENLAND025.L75_distbound.nc -nc4
    ```
    
    * Merging by itself is performed by mergebat, which take a [namelist](./namelist_mergebat)  as argument.
    
    ```
    mergebat.x -f ./namelist_mergebat
    cat ./namelist_mergebat
    &nammerge
      rn_limit  = 80    ! (km) Width of the ramp for merging
      ! Original file : The original bathymetry file to be patched 
      cn_orig  = "eGREENLAND025.L75_bathymetry.nc"
       cn_xdim  = "x"           ! x-dimension name
       cn_ydim  = "y"           ! y-dimension name
       cn_vbat  = "Bathymetry"  ! name of the bathymetry variable in original file
      !
      ! Patch file :
      cn_patch = "eGREENLAND025.L75_bathy_meter_002.6.nc"
       cn_vbatp = "Bathymetry"  ! name of the bathymetry variable in the patch file
      ! 
      ! Output file : (from a copy of the original file)
      cn_final = "eGREENLAND025.L75_bathy_meter_003.nc" 
      ! 
      ! Distance file : give the distance to the boudaries
      cn_dist = "eGREENLAND025.L75_distbound.nc"
       cn_vdist = "Tcoast"     ! name of the distance in the distance file
      /
     ```
  
  * Final step: patch regional file into the global file
  
  ```
  apply_patch.x  -w 898 1124 972 1204 -b eORCA025_bathymetry_b0.2_closed_seas.nc -p eGREENLAND025.L75_bathy_meter_003.nc -o eORCA025_bathymetry_b0.2_closed_seas_greenland.nc
  ```


  * Clean some intermediate files





