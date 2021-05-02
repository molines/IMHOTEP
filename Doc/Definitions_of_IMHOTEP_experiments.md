# Definition of IMHOTEP configurations

## 1. Context
A series of runs are foreseen during the IMHOTEP project.   
A first set of runs, based on a global eORCA025 configuration, is dedicated to the sensitivity
of the ocean  circulation to different runoff scenarii. The focus is put on 3 regions : (i) the 
tropical Atlantic, (ii) Greenland and (iii) the northern part of the indian ocean.  Runs in the 
first set are single member runs.   

In the second set of runs, 10 members, ensemble runs will be performed. For this purpose, a global 
eORCA05 configuration will be set up, and used as a mother grid for 2 AGRIF nests at 1/4 deg 
resolution, nests being located on the regions on focus. This latter ensemble configuration is very 
innovative for mixing multiple AGRIF nests in an ensemble run. Further more, one nest is located 
over the E-W periodic  boundary of the global configuration. 

## 2.  General road map:
 In order to achieve the goals of the project we plan to follow the following technical roadmap, 
regarding NEMO configurations:
 * Set up the eORCA025 mono member configuration
 * Perform first set of simulations
 * Set up an eORCA05 configuration
 * Add the Atlantic and Indian AGRIF nests
 * Add ensemble capabilities
 * Perform the second set of simulations.

## 3. eORCA025 configuration
### 3.1 Physical domain:
Decision was taken to use eORCA025 domain instead of ORCA025 (eORCA025 takes into account antarctic ice-shelves realistically):
  * This configuration is used in OMIP.
  * Freshwater discharge from Antarctica is much better representend and iceberg calving can be treated in a realistic way.
  * When using a great number of cores for computation, the over cost is reasonable as there are many land-only domain. The improvement
is mainly in the Ross Sea and Weddel Sea.
### 3.2 NEMO code:
We decided to use NEMO version 4.0.6
  * This is the last stable version of the NEMO 4.0.x series. It has already widely been tested and risks are limited with this version, which make
it suitable for this project.
### 3.3 Configuration files:
In NEMO 4.0.x, almost all configuration dependent adjusments have been eliminated. Those adjustments now appear in configuration files that are
prepared, in the pre-processing phase of a run.  There are many files used in a realistic configuration !  For the project, a particular effort 
has been made in the preparation of these files, aiming at the documentation of all the technical steps performed, in order to improve tracability.
In [this document](../eORCA025/Preparing_eORCA025.md), the full procedure is reported. Please refer to it for details.   In the following, we just
summarize the actions and decision taken.

#### 3.3.1 Domain_cfg (from coordinates and bathymetry and vertical grid):
This file describe the numerical grid, horizontally and vertically. We took horizontal parameters from P. Mathiot eORCA025.L121 configuration.
(see Pierre's  [documentation](https://pmathiot.github.io/NEMOCFG/docs/build/html/simu_eORCA025.html#monitoring) for all details). For the vertical grid
we choose the standard DRAKKAR's 75-levels.
  * Bathymetry :  eORCA025_bathymetry_b0.2_closed_seas_greenland.nc.  This file is basically Pierre's bathymetry, in which I remove the closed seas, and adjust the
bathymetry around Greenland, using BedMachineGreenland-2020-04-10.nc file (see details [here](../eORCA025/BUILD/GREENLAND-BATHY/README.md)
  * Coordinates :  eORCA025_coord_c3.0.nc
#### 3.3.2 Initial conditions and SSS restoring:
We choose World Ocean Atlas 2018 monthly climatology corresponding to the period 1981-2010 giving in-situ temperature and relative salinity.  For NEMO we infer potential temperature sigma-0.
#### 3.3.3 Forcing files (except runoff)
We choose JRA55 forcing data set.
  * This data set has the great advantage to cover the period 1958-present (updated each year).
#### 3.3.4 Other Auxiliary files:
Just to mention these files,  needed for running the configuration:
  * BFR2D file : precise location of enhanced bottom friction
  * SHLAT2D file : Locally specify the kind of lateral boundary condition (free-slip, no-slip, strong-slip)
  * RESTO : indicate the 3D restoring time scale for T and S (where there are !)
  * DISTCOAST : a file indicating the distance to the coast used to fade out SSS restoring near the coast.
  * IWM : Internal wave mixing input files, used in the Delavergne parameterization that we are going to use. Gives map of 5 variables (mixing power, and decay scales).
  * WEIGHT : Using Interpolation On the Fly (IOF) in NEMO for atmospheric forcing and geothermal fluxes, weight files used for the interpolation (either bilinear or bicubic) must be provided.
#### 3.3.5 Runoff files (specific to this project).
We used 3 sources for building the runoff files:
  * ISBA : daily runoff from 1979-2018. Only liquid runoff. For the climatological run, we computed the daily climatology and eliminate the runoff corresponding to Greenland and Antarctica.  
Data provided by Fabrice Papa, William Llowel. Projected on the NEMO grid with Julien Jouanno python tool.
  * GrIS : Greenland freshwater discharge provided by Jeremie Mouginot, Pierre Mathiot et Nicolas Jourdain. A climatology for the period  1950-1972 has been computed. The data are given on the *ad-hoc* NEMO
points, in the vicinity of 262 locations around Greenland where estimates of liquid and solid freshwater discharges  are computed from a mass-balance model.  The main problem is that most of the glaciers 
that feed the freshwater discharge had their termination upstream narrow fjords, not representend within the eORCA025 grid.  Another specific problem is also that there are bathymetric sills that prevent deep
fjords waters to connect to the open ocean. A consequence of these problems, is that part of the icebergs calving from the glacier termination melt into the fjords before reaching the NEMO point. An estimate
of 50% of melting of the icebergs in the fjords was taken.  Therefore GrIS data provide:
    * Solid freshwater flux (calving rate) : 50% of the quantity issued at the glacier front. This calving rate will be used for explicit representation of the icebergs in NEMO
    * Liquid freshwater fluxed corresponding to the melting of icebergs (50% of the quantity issued at the glacier front). This freshwater flux will be used in NEMO within the iceshelf representation (ISF),
associated with a particular depth range. Latent heat flux will be taken into account.
    * Liquid fresh water flux coming from inland surface waters : this flux will be used in NEMO as a river runoff.
  * RIGNOT et al. (2013) : For Antarctica, climatological annual data were already prepared by Pierre Mathiot, using Rignot et al estimates. They will be introduded in NEMO as :
    * Calving rate for NEMO-ICB module (located at the iceshelve edge).
    * Basal iceshelf melting for NEMO-ISF module, using Pierre Mathiot parameterization, introducing the basal melting as freshwater flux applied on a depth range, at the iceshelf edge.

  * Runoff depths for NEMO paramerizations are provided in a single file, obtained by merging all the data sources (fix in time!).

Modifications in NEMO were necessary in order to deal with multiple dataset (different frequency, different type --climatological or interanual--). This was done for runoff, calving and iceshelf data set.
    

### 3.4 namelists
  * Ocean : [namelist.eORCA025.L75-IMHOTEP00](../eORCA025/eORCA025.L75-IMHOTEP00/CTL/namelist.eORCA025.L75-IMHOTEP00)
  * Ice : [namelist_ice.eORCA025.L75-IMHOTEP00](../eORCA025/eORCA025.L75-IMHOTEP00/CTL/namelist_ice.eORCA025.L75-IMHOTEP00)

### 3.5 XML files where model output are fixed:
In our setting, NEMO model output are managed by the XIOS server, running asynchronously with NEMO. This provides a very flexible choice for the variables we want to output as well as the
output frequency. Furthermore, subdomains or sections can be output by theirself.  All the choices are passed to XIOS via the file `iodef.xml` that must be edited to fit our needs.

*TO BE DISCUSSED*

### 3.6 Scalability experiment.
A scalability experiment was perfomed on jean-zay, from 240 cores (6 nodes) to 2400 cores (60 nodes). For this experiment, we choose to use 4 xios server, running on a separated
computing node. 3 days runs (216 time steps of 1200 sec) were used, and we took the last day for evaluating the performance (measured as step/mn). On figure 1 we present the scalability diagram.

<img src=./Figures/scalability.png  width=130% />

On this busy picture, X-axis  corresponds to the number of cores used for NEMO.  Blue points, corresponds the actual performance (step/mn, left Y-axis), and red points 
are the equivalent, assuming a perfect scalability (with reference to the 280 core case). Brown points show the efficiency (%, left Y-axis), which is the ratio between actual and theoretical performances.  Except for some outliers, the efficiency is very close to (or above) 100%, until 1800 cores, but still very good up to 2400 cores, (85%). According to these
performances, yellow points indicates the elapsed time for 1yr of simulation (hours, right Y-axis), and green points the CPU hours for 1 year (hoursx10, left Y-axis). Due to the 
good scalability, the CPU hours are very stable (except for the outliers), around 2500 hours/years. In general, the performance decreases somehow when the model spins-up and when
the number of icebergs is stabilized. So, regarding this experiment, we can estimate  that 1year of eORCA025 experiment should not cost more than 3250 hours (taking a margin of 30%).

The sweet spot for this experiment is probably around 1500 cores, considering the trade off between queue wait-time and elapsed time. 

## 4. eORCA05 configuration

This configuration is not standard. I created eORCA05.L75 using eORCA025 horizontal grid. Once the grid is created, all configuration files where also created, using the same data sources than for eORCA025.
We start this action very early in the project in order to evaluate the feasability. In particular, AGRIF nests that are foreseen in the second stage of the project, cross the periodic line in the Indian Ocean. We wanted to implement an old idea, to shift the eORCA05 grid by 180 degree, so that the E-W periodic line stands in the Eastern Pacific, away from AGRIF nests ! 

I wrote a program to perform the shift on all configuration files and ... **IT WORKS !**  The shifted eORCA05 configuration was named eCAOR05.L75 (it is important to change the name, in order to avoid
confusions).

## 5. eCAOR05 configuration.
