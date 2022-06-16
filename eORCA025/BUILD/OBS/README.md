# Observation operator : OBS module
## Context:
OBS module is primarily used for data assimilation as it gives the model equivalent of an observation dataset. But
beyond data assimilation, this is also a powerfull tool for model-observation comparison.    
We decided to switch it on as it was done in OCCIPUT, using [ENACT-ENSEMBLE](https://www.metoffice.gov.uk/hadobs/en4/download-en4-2-1.html) dataset, which gather in monthly files, almost all the existing hydrographic profiles.   In OCCIPUT we also used JASON2 along track altimetric data. This latter data set is available since 2008. Unfortunatly, for IMHOTEP
WP1 we simply forgot to switch it on ...

## ENACT data set:
  * Data set was downloaded (4.2.1 version) from the UKmetoffice dedicated [website](https://www.metoffice.gov.uk/hadobs/en4/download-en4-2-1.htm). 
  * Original data are in a specifig ENACT format, no more supported by NEMO (since 4.0), and data reformating is
necessary before using them in OBS: NEMO now read only `feedback` format. 
    * OBSTOOLS distributed with NEMO offer a series of programs dealing with feedback data. In particular, `enact2fb.f90`
and `fbcomb.f90`:
       * **enact2fb.f90** transforms original ENACT data set to feedback format. A [enact2fb.sh](./enac2fb.sh) script
is given as an example of use for this reformating program.
       * **fbcomb.f90** is used for recombining all subdomain OBS file into a global feedback file
> Note that we had to compile OBSTOOLS for NEMO 4.0.1 as more recent revision is not in phase with NEMO (there are
> shared fortran module between NEMO and OBSTOOLS). 

## JASON data set:
  * AVISO provide the data set in annual feedback files, ready to use in NEMO/OBS

## Run time implentation: 
### Namelist setting
Namelist block `namobs` is used to pass all OBS relevant information to NEMO.  
Note `ln_diaobs   = .true.` and both `ln_t3d      = .true.`, `ln_s3d      = .true.` for using TS profiles.  
Data files are specified at runtime with `cn_profbfiles = ENACTFILES_LIST`. The keyword `ENACTFILES_LIST` 
is replaced by the corresponding list of files according to the period of simulation.


Similarly, for JASON data set, `ln_sla` should be set to true and data files specified by `cn_slafbfiles = SLAFBFILES_LIST`

```fortran
!-----------------------------------------------------------------------
&namobs        !  observation usage switch                              (default: OFF)
!-----------------------------------------------------------------------
   ln_diaobs   = .true.             ! Logical switch for the observation operator
   !
   ln_t3d      = .true.              ! Logical switch for T profile observations
   ln_s3d      = .true.              ! Logical switch for S profile observations
   ln_sla      = .false.             ! Logical switch for SLA observations
   ln_sst      = .false.             ! Logical switch for SST observations
   ln_sss      = .false.             ! Logical swithc for SSS observations
   ln_sic      = .false.             ! Logical switch for Sea Ice observations
   ln_vel3d    = .false.             ! Logical switch for velocity observations
   ln_altbias  = .false.             ! Logical switch for altimeter bias correction
   ln_sstbias  = .false.             ! Logical switch for SST bias correction
   ln_nea      = .false.             ! Logical switch for rejection of observations near land
   ln_grid_global = .false.          ! Logical switch for global distribution of observations  ! JMM 17/06/21
   ln_grid_search_lookup = .false.   ! Logical switch for obs grid search w/lookup table
   ln_ignmis   = .true.              ! Logical switch for ignoring missing files
   ln_s_at_t   = .false.             ! Logical switch for computing model S at T obs if not there
   ln_sstnight = .false.             ! Logical switch for calculating night-time average for SST obs
   ln_bound_reject  = .false.        ! Logical to remove obs near boundaries in LAMs.
   ln_sla_fp_indegs = .true.         ! Logical for SLA: T=> averaging footprint is in degrees, F=> in metres
   ln_sst_fp_indegs = .true.         ! Logical for SST: T=> averaging footprint is in degrees, F=> in metres
   ln_sss_fp_indegs = .true.         ! Logical for SSS: T=> averaging footprint is in degrees, F=> in metres
   ln_sic_fp_indegs = .true.         ! Logical for SIC: T=> averaging footprint is in degrees, F=> in metres
! All of the *files* variables below are arrays. Use namelist_cfg to add more files
   cn_profbfiles = ENACTFILES_LIST   ! Profile feedback input observation file names
   cn_slafbfiles = SLAFBFILES_LIST   ! SLA feedback input observation file names
   cn_sstfbfiles = 'sst_01.nc'       ! SST feedback input observation file names
   cn_sssfbfiles = 'sss_01.nc'       ! SSS feedback input observation file names
   cn_sicfbfiles = 'sic_01.nc'       ! SIC feedback input observation file names
   cn_velfbfiles = 'vel_01.nc'       ! Velocity feedback input observation file names
   cn_altbiasfile = 'altbias.nc'     ! Altimeter bias input file name
   cn_sstbiasfiles = 'sstbias.nc'    ! SST bias input file name
   cn_gridsearchfile ='gridsearch.nc' ! Grid search file name
   rn_gridsearchres = 0.5            ! Grid search resolution
   rn_mdtcorr  = 1.61                ! MDT  correction
   rn_mdtcutoff = 65.0               ! MDT cutoff for computed correction
   rn_dobsini  = 00010101.000000     ! Initial date in window YYYYMMDD.HHMMSS
   rn_dobsend  = 00010102.000000     ! Final date in window YYYYMMDD.HHMMSS
   rn_sla_avglamscl = 0.             ! E/W diameter of SLA observation footprint (metres/degrees)
   rn_sla_avgphiscl = 0.             ! N/S diameter of SLA observation footprint (metres/degrees)
   rn_sst_avglamscl = 0.             ! E/W diameter of SST observation footprint (metres/degrees)
   rn_sst_avgphiscl = 0.             ! N/S diameter of SST observation footprint (metres/degrees)
   rn_sss_avglamscl = 0.             ! E/W diameter of SSS observation footprint (metres/degrees)
   rn_sss_avgphiscl = 0.             ! N/S diameter of SSS observation footprint (metres/degrees)
   rn_sic_avglamscl = 0.             ! E/W diameter of SIC observation footprint (metres/degrees)
   rn_sic_avgphiscl = 0.             ! N/S diameter of SIC observation footprint (metres/degrees)
   nn_1dint = 0                      ! Type of vertical interpolation method
   nn_2dint = 0                      ! Default horizontal interpolation method
   nn_2dint_sla = 0                  ! Horizontal interpolation method for SLA
   nn_2dint_sst = 0                  ! Horizontal interpolation method for SST
   nn_2dint_sss = 0                  ! Horizontal interpolation method for SSS
   nn_2dint_sic = 0                  ! Horizontal interpolation method for SIC
   nn_msshc     = 0                  ! MSSH correction scheme
   nn_profdavtypes = -1              ! Profile daily average types - array
/
```

### Bug fix in NEMO
  * We had to fix a bug in `obs_prep.F90`: During model initialisation, NEMO looks for all available data in the
data files, corresponding  to the time window covered by the simulation 'segment'.  The correct time limits,
computed in the code were erased by values read in the namelist (and not updated). We have to move the  code 
lines computing the time limits after the namelist read.
  * **NOT FIXED** : we noticed that as it is written in the actual code, a strong assumption is done when computing
the time limits : time step should be the same since the begining of the **RUN**.  This is quite dangerous and should be fixed. (Presently, if time step is changed, there will be a mismatch between the model time and observation time).

## Issues during model production:
OBS module consumes a lot of memory, as all the available data (in files)  are put into RAM during the model
initialisation.  The amount of available data is increasing with time.  When we started the simulation (in 1958) we were able to proceed with yearly segments (which was very convenient as XIOS saved monthly and yearly means on the fly!).
This was OK till the end of 1977. In 1978 we had memory issues due to the increase of ENACT data. We therefore 
went to proceed with monthly segments and the memory issue disappeared. But we notice that restarting the model 
every month had a CPU cost (+15%). So we went to proceed with semestrial segments, and it was OK till 2004 
(June). Afterward we went back to monthly segments.  
Note that for all the IMHOTEP WP1 simulations (S GAI AI GA GI), we proceed always with monthly segments.

## Use of SLA from satelite altimetry
  * AVISO/CMEMS propose L3 dataset (along-track SLA) for all the satelites from TP to j3. The format of CMEMS file is not
compatible with NEMO/OBS operator, which requires `feedback` format.
  * Convertion program has been developped (aviso_fdbk.f90)[https://github.com/molines/JMMTOOLS/blob/master/DATA_TOOLS/FBK/aviso_fdbk.f90] for this purpose.
  * Monthly feedback files are archived on jean-zay at :
    * `/gpfswork/rech/cli/commun/DATA_SET/OBS/AVISO` and
    * `/gpfsstore/rech/cli/commun/DATASET/OBS/AVISO`
  * In ALL subdirectory, symbolic links have been done to the various satelites with a generic name, so that it can
be used easily in the production stream. Note that some adjustment is required in the RUNTOOLS in order to deal with
monthly SLA OBS files.
