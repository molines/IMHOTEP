# IMHOTEP WP2 : Ensemble simulations

## Overview
Some the the WP1 simulations will be replayed with ensembles, in order to unravel forced and intrisic variabilities. All ensembles run will be 
performed with the same code, the only changes being the freshwater forcing files, as in WP1. In this document,  information concerning the set up and 
production of ensemble simulations is given. The first 2 ensemble run are IMHOTEP.ES and IMHOTEP.EGAI, corresponding respectively to WP1 simulations 
IMHOTEP.S and IMHOTEP.GAI. For cost reasons, the ensembles are 10-members ensembles.

## Code
The code is the ensemblist version of NEMO@version 4.0.6, as it was used for  the deterministic simulation.  Details on how the code was ensemblized (mainly by Jean-Michel Brankart -- IGE ) are given in the [porting document](../../Doc/Ensemblization.md)

## Simulation paradigm
### creating initial spread
  * All members start from a single restart file coming from WP1 spinup run (eORCA025.L75-IMHOTHEP2) at 1974/12/31.
  * Stochastic parameterization for the Equation of state is used for year 1975, then switched off for the following years.
  * OCCIPUT stochastic parameters were used for IMHOTEP.
  * Restart files (10 members) at 1979/12/31 will be used for the counterpart eORCA025.L75-IMHOTHEP.EGAI. 
### atmospheric forcing
  * In order to relax constraint on the ocean surface fields, ensemble average of the atmospheric/ice forcing  was used for all members (only
    heat and freshwater fluxes are concerned, momentum fluxes are kept individual for each members, using the CFB parameterization (Renault et al).
  * This technique leads to instabilities in the sea ice model (see below), and we end up with using the ensemble forcing average only where 
    all members are ice-free.
### Data production
####  Naming of the files.
  For ensemble run, there is a need to identify member number in the file names. The rule instaured during OCCIPUT is used : The member number is coded
as a variant of the CASE name, appending .<MBR> to the CASE name, <MBR> representing the member number with 3 digits. Ex: eORCA025.L75-IMHOTEP.ES.007,
corresponding to member 7 of the IMHOTEP.ES simulation.
#### Gridded data set
  * Use of XIOS for 1mo and 1d model output. 
  * For monthly (1m) output, we save the second order momemts (UT, VT etc...) average computed from each time step values.
  * At the end, we restrict the data set archive to 5d 3D fields, instead of 1d. But XIOS cannot be used for saving data at both 5d and 1mo, because
    each segment of the run should last and integer multiple of the output frequency, which is not possible with a true calendar.
#### Observatinnal data
  * from 1980 to the end of the run, EN4 observations were collocalized for each member (OBS profiles operator).
  * from 1993 to the end of the run, SLA from atlimetric missions collocalized for each member (OBS SLA operator).   Used satelites used during this
    period are : Topex/Poseidon (01/1993 - 04/2002   ), jason-1(05/2002 - 07/2010 ), jason-2 (08/2010 - 05/2016 ) and jason-3 ( 06/2016 - 12/2020). 
    Resulting synthetic observation files are saved under OBS directory.
#### Iceberg trajectories
  * As for WP1 runs, icebergs are explicitely represented in the simulation via the ICB module. Each member will produce its own set of icebergs
    trajectories, saved in ICBTRJ directorY.

## Issues
When starting the production of thre run, some issues were raised concerning :
  * iceberg restart files when changing the domain decomposition
  * Altimetric observational data in OBS operator: NEMO bugs encountered.
  * Issues concerning the ensemble mean forcing, regarding sea ice model.

## Specific tools developped for this run
### `FeedBack` formating of the altimetric data from AVISO
### Spliting iceberg restart files
### Performing data reduction from daily output to 5d average (VVL case).



