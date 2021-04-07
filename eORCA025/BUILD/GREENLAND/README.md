# Fresh water fluxes from continental Greenland


## 1. Context:
In the IMOTHEP project, the fresh water flux coming from Greenland is one of the topic to be treated, in order to
estimate the impact of the variation of this flux on the ocean circulation and sea level.

The Greenland fresh water flux (GFWF) has a liquid (melted glacial waters, basal melting) and a solid (calving) contribution. Jeremie Mouginot, Pierre
Mathiot and Nicolas Jourdain provided a file with 262 points around Greenland (corresponding to eORCA025 model cells), with the monthly liquid 
and solid discharges.  In addition, a depth corresponding to the base of the glaciar or the the depth of bathymetric sills in the fjords, is given and will
be used in NEMO to apply the runoff at depth.

Due to model resolution, most of the Greenland Fjords are not explicitely represented in the configuration. For the solid contribution,
it is known that a certain amount of the calved iceberg from the glaciar front, just melt in the fjord before reaching the open ocean. 
There are some estimate (quite few indeed) published in the litterature, giving the proportion of icebergs reaching the oceans over the total
amount of calved icebergs at the glaciar front.  This proportion is within a range of 30% to 80%.  In order to go ahead, a decision was taken to
make a first GFWF data set, assuming that 50% of the calved icebergs melt in the fjords, thus converting the equivalent solid discharge to liquid discharge.

For setting up the GFWF file, a very detailed cartography (150m resolution) of Greenland, giving the elevation of the bed-rock, the thickness of the ice etc...
This is the BedmachineGreenland data base.  Jeremie used the last version (2020.04.10), not yet published of this file. On the other hand, the eORCA025.L75 
NEMO configuration has already a bathymetric file.  Comparing the runoff depths given in the GFWF data-set (infered from BedMachine) and the actual eORCA025.L75
bathymetry, large discrepancies were encountered, whith 127 model points having a depth much smaller than the runoff depth. For coherancy, we decided to review
the model bathymetry around Greenland, using the BedMachineGreenland-2020-04-10.nc file.

## 2. Procedure used for building a new improved bathymetry around Greenland

### 2.0  Required software: 
  * [NEMOBAT](https://github.com/molines/NEMOBAT.git) and in particular: 
    * [NSDIC_map_trf.f90](https://github.com/molines/NEMOBAT/blob/master/INPUT_UTILITIES/NSIDC_map_trf.f90), used for transforming stereo graphic polar
coordinates to longitude latitude.
    * [bathyinterp.F90](https://github.com/molines/NEMOBAT/blob/master/INTERP0/batinterp.F90), used to interpolate refined bathymetry on the NEMO grid.
    * [mergebat.f90](https://github.com/molines/NEMOBAT/blob/master/INPUT_UTILITIES/mergebat.f90), used to patch the bathymetric file with the regionally
refined one.
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

Note that we choose nn_interp=1 (Median average) as the interpolation method.
 


