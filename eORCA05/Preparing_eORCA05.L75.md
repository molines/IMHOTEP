# Preparing eORCA05 configuration
## Context
In the series of the ORCA configuration, ORCA05 has never been extended to eORCA05. The only operational configuration is ORCA05.L46.
In the second part/phase of the project, we plan to run an ORCA05 configuration with 2 AGRIF nests (in the North Atlantic and North Indian oceans), with a refinement factor of 2, leading to have localy 1/4 degree resolution, identical to the resolution used in the first part of the project. In the second part, we 
plan to use this nesting strategy because we want to perform an ensemble run with 10 members. Using nests will reduce the cost of the ensemble simulation.

It is of primary interest for the project to be able to compare runs achieved in the two phases. As be are using eORCA025.L75 in the first phase, it seems
logical to setup an eORCA05.L75  as the mother grid used in the second phase.  An extra difficulty comes from the location of the North Indian nest : it crosses
the E-W periodic boundary of the ORCA grid, which is just not supported by NEMO v4.0.6 (used in the project). We manage to shfit the E-W periodic boundary 
by 180 deg (which became located in the Eastern Pacific), so that both Atlantic and Indian oceans are not splitted.

This document describes the actions that were performed for the development of the  eCAOR05.L75 configuration, used as the mother grid for the AGRIF nests.

## Creating the eORCA05 horizontal grid.
We start from eORCA025 grid and figure out how ORCA05 points matches  eORCA025  points.  We found that T point ORCA05(3,256) is located as T points eORCA025(5,697).
In the same manner, for instance, we found that U point ORCA05(3,256) = T points eORCA025(6,297), etc ..
The program [mkorca05.f90](BUILD/HGR/mkorca05.f90) was written in order to create the eORCA05 grid, and in particular its southern extension, using  the matching points.
After determining the location of T U V F points in the eORCA05 grid, e1 and e2 metrics was calculated using orthodromic distance between *ad hoc* points. Finally, we 
patch the eORCA05 grid with original ORCA05 grid north of J=79.  This program ends up creating the file `eORCA05_coordinates.nc`.

## Creating the eORCA05 bathymetry.
Here again we use the eORCA025 bathymetry to infer eORCA05 bathymetry, ice-shelf bathymetry and ice-shelf draft.  eORCA05 bathymetry is a weighted average of
9 eORCA025 neigbour points of the matching T points.  The program [mkbathy05.f90](BUILD/HGR/mkbathy05.f90) was build for this purpose. As for the coordinates,
the eORCA05 bathymetry coming out from this process, was patched with the original ORCA05 bathymetry, north of J=79. This program ends up creating the `eORCA05_bathymetry_b0.2_closed_seas.nc`

## Creating the eORCA05.L75 domain_cfg
Having now both the coordinates, and the bathymetry for the eORCA05 horizontal grid, we were able to compute the `domain_cfg.nc` file using exactly the same
settings that were used for eORCA025.L75 (see the procedure in [this document](../eORCA025/BUILD/DOMAIN_cfg/README.md) ).  We end up with the file `eORCA05.L75_domain_cfg.nc`.

## Creating Initial conditions for T and S, as well as the restoring T and S: (Potential temperature and Relative salinity).
We choose the same WOA18 data set that was used for eORCA025.L75 (30 yrs climatology, 1980-2010), and the same `SOSIE` procedure. So, all the sosie namelist used with eORCA025.L75 were adapted to eORCA05.L75.
The adaptation only consists in changing the name of the mesh_mask.nc file. The output files produced by SOSIE were reformatted to a NEMO standard, using the 
[mknemolike.sh](BUILD/INITIAL_COND/mknemolike.sh) script. This latter operation is basically  dedicated to renaming the netcdf file dimensions, and coordinates 
variables. The process ends up producing  `eORCA05.L75_81B0_WOA18_1m_votemper.nc` and `eORCA05.L75_81B0_WOA18_1m_vosaline.nc`

## Creating weight files for the atmospheric forcing.
Weights file for JRA55 forcing data set were prepared  following the same procedure that was used for eORCA025 (see [this document](../eORCA025/BUILD/WEIGHTS/README.md) ).
When calling the `mkweights.ksh` scripts, of course we used the eORCA05_domain_cfg.nc file. `wght_JRA55_eORCA05_bicub.nc`  and `wght_JRA55_eORCA05_bilin.nc` were build 
this way.

## Creating Continental Fresh Water fluxes files:
### **Runoff**
### **Calving**
### **Iceshelf melting**
### **SSS restoring climatology:**  
In the first phase of the project,  spinup run is used  to infer a precipitation correction corresponding to the climatology of 
the SSS restoring. All sensitivity runs are performed without any SSS restoring, but using this correction.  When switching to global eORCA05, the question
is raised wether to use the same approach (requires a eORCA05 spinup, and diagnostic of the SSS restoring term), or to directly use an eORCA05 interpolation of the correction.
Behind this question, is the coherency regarding the total amount of fresh water entering  the domain. (Still, evaporation is not controlled externally and may differ between 
the 2 resolutions !). 

## Creating extra config files
### **Distance to coast**
### **Enhanced bottom friction**
### **Lateral conditions (shlat)**
### **3D restoring coefficient.**


## From eORCA05.L75 to eCAOR05.L75 : shifting the grid
A specific program, written as a new [CDFTOOLS](https://github.com/meom-group/CDFTOOLS), `cdfshift05.f90`, just shift the E-W periodic boundary by 180 degree. The progra m is fair enough to dealwith any NEMO input file, provided the name of 'x' and 'y' dimensions are known, and the time dimension (if any) is `UNLIMITED` 
in the netcdf sense. Resulting CAOR05 grid and files  have been successfully tested. This was a great step forward for the implementation of phase 2 with AGRIF nest !
