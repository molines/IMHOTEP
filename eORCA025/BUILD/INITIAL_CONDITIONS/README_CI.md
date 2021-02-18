# Building Initial condition step by step
The building of initial conditions suppose that you already have :
  * domain_cfg file for the configuration (see [how to build this file](BUILD/DOMAIN_cfg/README.md) ).
  * mesh_mask files for the configuration: Once the domain_cfg file is OK, the best way to obtain a `mesh_mask.nc` file is to run 
NEMO with `ln_meshmask=.true.` in the namelist. This job will crash almost immediatly because of so many missing files ... but at least will produce the mesh_mask.nc file ! An extra recommendation for getting mesh_mask.nc file is to run NEMO without land-only sub domains (forcing for instance `jpni=1` in the NEMO namelilist (nammpp)). Doing so, variables in the mesh_mask.nc file are defined everywhere in the global domain, which is very useful.
  * TS 3D-data set that will be interpolated to the model grid. See below how we manage to get a valid data set.
  * A last version of [SOSIE3](https://github.com/brodeau/SOSIE), ready to be used. 

## 1. Getting the climatological data
### 1.1 Recent version of World Ocean Atlas : WOA2018
  * We took the TS climatology from the NOAA data center. 
  * This raw data set requires some adaptation before using it for interpolation.
  * See details of the preparation in this [report](../WOA2018/WOA18_processing.md)
### 1.2 Other data set that can be considered
  * EN4
  * older WOA atlas
  * Gouretsky
  * ARGO based data set.

## 2. Interpolation with  SOSIE3
### 2.1 Get a recent version of SOSIE3  and compile

```
     cd YOUR_FAVORITE_DEV_DIRECTORY
     git clone https://github.com/brodeau/SOSIE  SOSIE3
     cd  SOSIE3
     ln -sf macro/make.macro_ifort_JEAN-ZAY make.macro
     # or choose in macro the adhoc macro file (or create it).
     # This file is machine and compiler dependent
     # compiling options and path to netcdf libraty are indicates there.
     make
     
```

### 2.2 producing the NEMO files

For eORCA025.L75-IMOTHEP00 we use :
|  files        | name                                   |
| ------------- |:--------------------------------------:|
| domain_cfg    | eORCA025.L75_domain_cfg_closed_seas.nc |
| mesh_mask     | eORCA025.L75_mesh_mask.nc              |
| T-file        | woa18_decav_theta01-12_01_depth1.nc    |
| S-file        | woa18_decav_san01-12_01_depth1.nc      |

SOSIE takes a single namelist as argument. The namelist should be carefully adapted to your specific case (input data, output grid). 
Namelists used for the initial conditions production are gathered [here](./). 

[namelist_theta_1deg](./namelist_theta_1deg) : namelist for potential temperature interpolation
[namelist_san_1deg](./namelist_san_1deg) : namelist for relative salinity interpolation

### 2.3 extra cleaning of the NEMO files
Although not mandatory, I like to have initial condition files following almost exactly the nomenclatura of the NEMO output. So far, I take care of having dimensions named
x,y,deptht, time_counter as well as the 'coordinates' variables nav_lon, nav_lat, deptht, time_counter. 

Doing do allow the use of CDFTOOLS on these files without any problem. 

> TIP : this extra cleaning is based on nco tools. However, when input files are NetCdf4 (which is the case), renaming dimension and variable may lead to corrupted files.
The script I made for this cleaning transform netcdf4 file to netcdf3 then perform the modifications, and finally come back to netcdf4 with chunking and deflation level of 1.
