# From single run to ensemble run: NEMO modification.
## Overview
Since already many years, we have used ensemble simulations with NEMO for various purposes. The code modifications 
have been initiated for assimilation purposes, by Jean-Michel Brankart. The modified code, has been used with some 
modifications for the OCCIPUT project aiming at disatangling intrisic and forced variability of the ocean, by using 
an eddy-permitting ensemble simulation. The modified code allows the production of an ensemble by running a single 
executable performing various members in parallell. This specific feature allows the possibility of an internal 
communication between various members at run-time. This possibility is used for building unique forcing fields 
for all member, as shown below.

In this document, NEMO modifications related to ensemble simulations are detailed, for NEMO version 4.0.6. 

## Code modification: Base
These are the core of the modification for running ensemble run, introduced in NEMO 4.0.2 by Jean-Michel.

For DCM version, all modifications related to ensemble will be introduced only when key_drakkar is defined at compile time. 
Is it necessary to have also a new CPP key such as key_drakkar_ensemble ? No clear idea, but for porting purpose I will 
introduce this key, that eventually will be merged with key_drakkar in order to leave only one drakkar key. 

We decide to keep the nomemclature that what used for OCCIPUT for naming the ensemble (this introduces differences with Jean-Michel code).
Following DCM good practice, an experiment is defined by the config name and the case name `<CONFIG>-<CASE>`. (Example eORCA025-IMHOTEP.S ).
The ensemble nomenclature used in OCCIPUT, encode the member number into the CASE part of the name, leading to `<CONFIG>-<CASE>.<MBR>` (where
`<MBR>` is a 3 digit member number (001 ... 010 etc.. ) (Example : eORCA025.L75-IMHOTHEP.ES.007 for the 7th member of run IMHOTEP.ES). The idea
is to be able to consider a single member as a particular CASE of the simulation (hence using standard tools will be OK). To implement this
nomenclature, for model output files, this will be coded in the xml files used by XIOS. In the fortran code, this have an impact on the files
directly written by NEMO such as restart files, ocean.output files and all the log files produced by NEMO at runtime.

### New control variables in namelist
New control variables are introduced in order to control the ensemble run. In order to be orthogonal to the standard code, modification
are introduced in the nammpp_drk new namelist block:

```fortran
!-------------------------------------------------------------------------
&nammpp_drk
!-------------------------------------------------------------------------
    ln_ensemble   = .false.     ! using ensemble T or F
    ln_ens_rst_in = .false.     ! members read their own restart ( T or F)
    nn_ens_size   = 10          ! number of members in the ensemble
    nn_ens_start  = 1           ! number of the first member
    ln_ens_diag   = .false.     ! Set up a member MPI-communicator allowing inter-member comm. (T or F )
/
!-------------------------------------------------------------------------
```

Note that `ln_ens_diag` flag just mean that there is a need for intermember communication. No specific diags are activated by this flag. Therefore,
it must be set to true when diags or communication between members (forcing issue for example) is required.

### Introduction of specific routines in lib_mpp.F90
  * `mpp_start_ensemble:` This routine is called from `nemogcm` at nemo initialisation. It returns `ilocal_comm`, a communicator associated with
each member and used to run NEMO.  This routine is  a driver that call `mpp_set_ensemble`. 
  * `mpp_set_ensemble:` When running NEMO in parrallel, an overall communicator is created which has the size equal to the number of mpi_task 
for NEMO.  When using ensemble run with several members, this overall communicator is divided into sub-communicator associated to each member. This
routine perfoms this splitting into member communicators, using the information provided in the namelist (number of members for instance).  
In addition, if `ln_ens_diag` is true, it defines an intermember communicator (in fact an array of communicator, with size jpproc (number of 
nemo task per member) )  allowing communication between all members on a subdomain. 

### Differentiation of file names according to member number.
When performing ensemble runs, all members run in parallel and should use files (restart file, log files, output files ... ) with an unambiguous name.
Therefore, the member number must appear in the relevant file names. 

We decide to keep the nomemclature that what used for OCCIPUT for naming the ensemble (this introduces differences with Jean-Michel code).
Following DCM good practice, an experiment is defined by the config name and the case name `<CONFIG>-<CASE>`. (Example eORCA025-IMHOTEP.S ).
The ensemble nomenclature used in OCCIPUT, encode the member number into the CASE part of the name, leading to `<CONFIG>-<CASE>.<MBR>` (where
`<MBR>` is a 3 digit member number (001 ... 010 etc.. ) (Example : eORCA025.L75-IMHOTHEP.ES.007 for the 7th member of run IMHOTEP.ES). The idea
is to be able to consider a single member as a particular CASE of the simulation (hence using standard tools will be OK). To implement this
nomenclature, for model output files, this will be coded in the xml files used by XIOS. In the fortran code, this have an impact on the files
directly written by NEMO such as restart files, ocean.output files and all the log files produced by NEMO at runtime. The following fortran modules
have been modified in this sense:
  * `domain.F90:` In this module, we fix directory and file names for ocean restart files. This module was already modified in the frame of DCM, 
as it allows the specification of a particular directory for restart files (in and out) and also allow to produce restart files names ready for 
restarting a new segment without renaming the file (as in the standard code). For ensemble run, this module is modified so that the restart 
directory is : `<CN_OCERSTDIR>.<SEG>/<MBR>` where CN_OCERSTDIR is specified in the namelist (and set in the RUNTOOLS), SEG is the segment number and MBR 
the member number. Note that the run script is modified to take care of the creation of the MBR sub directory.  
The restart root filename  is : `<CN_RSTNAME>-<SEG>.<MBR>` where CN_RSTNAME is set in the namelist (and set in the RUNTOOLS), SEG and MBR as above. 
At the end of a segment, there will be a restart file for each sub-domain and each member. For instance, in directory `eORCA025.L75-IMHOTEP.ES-RST.25/007/`
restart file (for the ocean) restart-25.007_0234.nc. This latter file correspond to subdomain 0234, segment 25 member 007.    
  *  **TBD : this is also needed for all other restart (ice, icebergs ..**
  * `icestp.F90` modification for restart_ice file name, just as for the ocean above.
  * `obs_wri.F90` modification for the OBS output file name, appending `.MBR` at the end of the file name.
  * `nemogcm.F90` modification for ocean output
  * `stopar.F90` modification for stochastic restart files. Stochastic restart files are used to continue a simulation when stochastic perturbations are
being used. 
  * `stpctl.F90` modification for time-step file and run.stat as well as run_stat.nc
  * icebergs treatment:  ICB module produces specific files for trajectory and for restart. File names of both types must be modified in order to take the
member number into account. Restart files are written in the same directory than ocean restart files and as far as the directory name is concerned,
the modification for ensemble run is already done.  For trajectories, a root name for directory is passed via the namelist and I suggest to proceed 
as it has been done for restart: putting the ICB trajectory files into member sub directory. So modifications occur in 
    * `icbini.F90`:  In this routine, the namberg_drk namelist block is defined and read. This block hold `cn_icbrst_in`, `cn_icbrst_out` and `cn_icbdir_trj`.
This latter variable gives the path of the trajectory files (root name is `<CN_DIRICB> = <CONFIG>-<CASE>-ICB.<SEG>` (set by the nemo4.sh runtool)). In order to be coherent
with restart directories with ensemble run, we aim at having a directory name for member `<MBR>` to be `<CN_DIRICB>/<MBR>`. This implies the creation
of the `<MBR>` sub directory in the runtool script.  
For restart files, we endup with file name looking like `<CN_ICBRST>-<SEG>.<MBR>_<RANK>.nc` 

### Impact on stochastic parameterization.
Apart from name differentiation in stopar.F90, in case of ensemble run, a call to sto_par is performed in step.F90. Consulting with Jean-Michel, he suggests
to have this call systematically, independently of the use of stochastic parameterization, as it reduces to an empty loop if not needed.   
Note that for the project, at this level we only port stochastic parameterization related to the equation of state.


## Code modification: Ensemble forcing
The idea is that all members use a common atmospheric forcing, computed as the mean of the individual members forcing. The implementation of this idea
is done in trasbc.F90. In NEMO workflow, a call to SBC is done for all members, setting up heat forcing, fresh water forcing and momentum forcing.
Then the forcing is taken into account for tracers in trasbc.F90 and for momentum in in dyn_zdf.F90.   
In OCCIPUT we only dealt with heat and fresh water forcing, by adding a member average in trasbc.F90, just before the forcing is used. For IMHOTEP, we imagine
that the wind stress may also be averaged through members, in order to have a common momentum forcing.  This raises additional question as the wind stress module 
is also used in other routine such as zdftke (vertical mixing using tke). Other potential issues to check : atmospheric stress on sea-ice....

## Code modification: Model output and XIOS related modifications.
