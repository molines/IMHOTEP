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
  * `icestp.F90` modification for restart_ice file name, just as for the ocean above.
  * `obs_wri.F90` modification for the OBS output file name, appending `.MBR` at the end of the file name.
  * `nemogcm.F90` modification for ocean output
  * `stopar.F90` modification for stochastic restart files. Stochastic restart files are used to continue a simulation when stochastic perturbations are
being used. We decided to store/read  them in the same directory (out/in) than the ocean restart files. With ensembles, there is a subdirectory per member.
The name of the restart file follow the same construction as the ocean or ice restart files.
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
is also used in other routine such as zdftke (vertical mixing using tke). Other potential issues to check : atmospheric stress on sea-ice. A discussion
around this topic leads to the decision **NOT** to use ensemble average for wind stress, the main rationale being that we do want to have the Renault Current Feedback ON. 

The implementation of this ensemble forcing, needs the introduction of a new routine, available if ln_ens_diag is true: `mpp_ens_ave_std` that computes the ensemble average and 
ensemble standard deviation. This routine is then used in trasbc.F90 in order to replace the heat flux forcing by the ensemble mean.  

We introduce a specific flag in the namelist `nammpp_drk`, `ln_ens_forcing` which needs to be T if the ensemble forcing is desired. Important to note that
if ln_ens_forcing is true, then ln_ens_diag must also be true. A sanity check is implemented just after the namelist
is read.

## Code modification: Model output and XIOS related modifications.
XIOS needs an input file describing the data to be written (variables, frequency, grid etc...). This file uses
the xml format and is named `iodef.xml`. In this file,  `context`  environment for XIOS and for NEMO are defined.
For XIOS, it reduces to few lines defining xios internal variables. For NEMO, in case of a standard run, there is the
inclusion of a `context_nemo.xml` file, that will be detailed below. In case of ensemble run, the modification consists in adding as many
nemo context files as member. Therefore in the production stream, we must define a unique contex file per member. For instance; in iodef.xml
we used to have:

  ```
  <context id="nemo" src="./context_nemo.xml"/>       <!--  NEMO       -->
  ```

that will  be modified to

  ```
  <context id="nemo.001" src="./context_nemo.001.xml"/>       <!--  NEMO       -->
  <context id="nemo.002" src="./context_nemo.002.xml"/>       <!--  NEMO       -->
  <context id="nemo.003" src="./context_nemo.003.xml"/>       <!--  NEMO       -->
  ......
  <context id="nemo.010" src="./context_nemo.010.xml"/>       <!--  NEMO       -->
  ```

Note that the context id is recognized in NEMO, and the ensemble version is waiting for those id, formed with the member number : nemo.001 etc..


### Context file : context_nemo.xml
This file describe a particular context, identified by its id. In the context there are different parts:
  * variables definition : physical constants used by xios for some diags
  * fields definition : they are defined by importing `fiels_def_nemo-oce.xml` for the ocean and `fiels_def_nemo-ice.xml` for the sea ice. Any of the
possible NEMO output (ie corresponding to an `iom_put` statement in NEMO) must be defined in this file.  *A priori* it does not depend on members.
  * axis definition : they are defined by importing `axis_def_nemo.xml`.  This is the place where vertical axis, ice category axis, float number etc...
are defined. In a word, all axis which are not the horizontal ones (x and y). Variables described in field, have an associated axis. Axis are
independent from members in an ensemble run.
  * domain definition : They are defined by importing `domain_def.xml` file. They refer to horizontal domain or subdomain of the computational grid.
For example this is the place where specific 'boxes' or section are defined. Then this information can be used in the file_def file in order to
produce model output on a restricted area.  It is unclear to me (JMM) how this file is used for the global domain. There are probably domain
definition that are set directly from  NEMO, independently of the xml file.  Domain definitions are not member depending.
  * grid definition: They are defined by importing `grid_def.xml` file. Grids are defined by the association of a domain (horizontal) and axis (vertical
or other). There can be more than one axis for a given grid, although not frequent (example of grid `grid_znl_T_3D` which handle 3D zonal mean, with a 
a vertical axis and a basin axis). So far grids are independent from members. 
  * file definition:  They are defined by importing the user defined files `file_def_nemo-oce.xml` and `file_def_nemo-ice.xml`. These files
defined the output data plan, made by the user (which variables, which domain, which frequency). They might differ from member to member (for instance if
we decide to output ensemble mean computed on the fly, only one member will do the output). They can also differ when using DCM because the output file
names are defined in these files, including the output directory (DCM extension).   
In order to simplify the managment of file_def xml files, we opt at having a single file for all members. This requires the following
    * If ensemble mean are to be saved, the corresponding fields are defined in fields_def and in file_def, for all members. In NEMO, only one member
(for exemple 001 --why not ? -- ) with make the corresponding call to iom_put. 
    * We modify NEMO so that the keyword `@dirout@` is recognized in `iom.F90` and replaced by the absolute path of the xios output files. Therefore
we also add a new character variables in the namelist (namblock namrun_drk) : `cn_dirout`. This latter variable gives the root pathname of the output files,
and the segment number will be added in NEMO, as well as a member sub directory in case of ensemble run. (Just like for the restart files).  For example,
xios output files (prior any recombination) will be in directory $TMPDIR/eORCA025.L75-IMHOTEP.ES-XIOS-35/007/ (for segment 35 and member 007). Note that 
in this example, `cn_dirout` corresponds to the path `$TMPDIR/eORCA025.L75-IMHOTEP.ES-XIOS` and will be set in the namelist by the runtools.

##  RUNTOOLS modification:
RUNTOOLS were almost prepared for ensemble run since OCCIPUT. Some directory definitions have been added for iceberg for instance.
However, the management of xml file is completely different than for OCCIPUT (xios_1.0), and much simpler, by the way. Among the differences, is that every 
member use the same `file_dev_xx.xml` file, only the (small) context files are proper to each member.   This means that for instance, if intermember diagnostics
are to be output, the rule is to define in the code (NEMO) which member does the corresponding `iom_put`.

RUNTOOLS creates the different context files from a template file `context_nemo_MBR.xml` and the customization is reduced to change the `context_id`.  The 
`iodef.xml` file (which in fact is the direct XIOS input file) is created from template `iodef_MBR.xml`. As said above, the modification in iodef.xml is to 
add as many context lines as members. RUNTOOLS takes this in charge. I had to add some specific words for the script to recognize where to add the lines (START END).
Below is the actual `iodef_MBR.xml` file.

```
<?xml version="1.0"?>
<simulation>

<!-- ============================================================================================ -->
<!-- XIOS context                                                                                 -->
<!-- ============================================================================================ -->

  <context id="xios" >

      <variable_definition>

          <variable id="info_level"                type="int">10</variable>
          <variable id="using_server"              type="bool">true</variable>
          <variable id="print_file"                type="bool">true</variable>
          <variable id="using_oasis"               type="bool">false</variable>
          <variable id="oasis_codes_id"            type="string" >oceanx</variable>

      </variable_definition>
  </context>

<!-- ============================================================================================ -->
<!-- NEMO  CONTEXT add and suppress the components you need                                       -->
<!-- ============================================================================================ -->
<!--  START    -->
  <context id="nemo" src="./context_nemo.xml"/>       <!--  NEMO       -->
<!--  END      -->

</simulation>

```

