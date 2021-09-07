# IMHOTEP.GI
## Description
This experiment uses the exact same code than IMHOTEP.S. The same correction term is added to the precip and no SSS restoring is used.

## Continental Fresh Water Fluxes:
All runoff are interannual except for the largest tropical rivers in the Atlantic basin (Amazon-Orenoque, Niger-Congo)  which  are climatological.
### Code modification

In namsbc_rnf_drk corresponding runoff file is GI_runoff_ISBA_noAA_noGR_clim_366.nc. Note that for this experiment (and also the GA experiment), runoff
code has been sligthly modified to become :

```fortran
             rnf(:,:) = 0._wp
             DO ji = 1, nn_rnf_freq
               ! we use the additional file instead of the first one where is is non 0
               WHERE ( sf_rnf(ji)%fnow(:,:,1) /= 0. ) rnf(:,:) = sf_rnf(ji)%fnow(:,:,1)
             ENDDO
             rnf(:,:) = rn_rfact * rnf(:,:) * tmask(:,:,1)  ! updated runoff value at time step kt

```
### corresponding namelist
Note that with this trick, the last read data set replace the first ones: order of the data set does matter in the namelist !  
In our case, `GA_runoff_ISBA_noAA_noGR_clim_366.nc` file is specified as the last one. In the namelist :
  * River runoff

```fortran
!-----------------------------------------------------------------------
&namsbc_rnf    !   runoffs                                              (ln_rnf =T)
!-----------------------------------------------------------------------
   ln_rnf_mouth  = .false.     !  specific treatment at rivers mouths
      rn_hrnf    =  10.e0     !  depth over which enhanced vertical mixing is used    (ln_rnf_mouth=T)
      rn_avt_rnf =   2.e-3    !  value of the additional vertical mixing coef. [m2/s] (ln_rnf_mouth=T)
   rn_rfact      =   1.e0     !  multiplicative factor for runoff
   ln_rnf_depth  = .true.     !  read in depth information for runoff
   ln_rnf_tem    = .false.    !  read in temperature information for runoff
   ln_rnf_sal    = .false.    !  read in salinity information for runoff
   ln_rnf_depth_ini = .false. ! compute depth at initialisation from runoff file
      rn_rnf_max  = 5.735e-4  !  max value of the runoff climatologie over global domain ( ln_rnf_depth_ini = .true )
      rn_dep_max  = 150.      !  depth over which runoffs is spread ( ln_rnf_depth_ini = .true )
      nn_rnf_depth_file = 0   !  create (=1) a runoff depth file or not (=0)
   ln_rnf_icb  = .false.   !  read in iceberg flux from a file (fill sn_i_rnf if .true.)

   cn_dir      = './'      !  root directory for the runoff data location
   !___________!_________________________!___________________!___________!_____________!________!___________!__________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' ! filename ! pairing  !    filename   !
   sn_rnf      = 'eORCA025_runoff_ISBA_noAA_noGR' , 24.      , 'sorunoff', .true.      , .false., 'yearly'  , ''       , ''       , ''
   sn_cnf      = 'eORCA025.L75_rnf_msk'  ,         -12.      , 'socoefr' , .false.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_s_rnf    = ' '                     ,          24.      , 'rosaline', .true.      , .true. , 'yearly'  , ''       , ''       , ''
   sn_t_rnf    = ' '                     ,          24.      , 'rotemper', .true.      , .true. , 'yearly'  , ''       , ''       , ''
   sn_dep_rnf  = 'eORCA025.L75_rnf_dep ' ,           0.      , 'sozisfmax',.false.     , .true. , 'yearly'  , ''       , ''       , ''
   sn_i_rnf    = 'NOT_USED'              ,        -1.        , 'sorunoff', .true.      , .true. , 'yearly'  , ''       , ''       , ''
/
!-----------------------------------------------------------------------
&namsbc_rnf_drk  !   runoffs  drakkar multiple file enhancement         (ln_rnf =T)
!-----------------------------------------------------------------------
   nn_rnf_freq = 3
   !___________!_________________________!___________________!___________!_____________!________!___________!__________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' ! filename ! pairing  !    filename   !
   sn_rnf2(1)  = 'eORCA025.L75_1m_greenland_rnfbis' , -1.    , 'sorunoff', .true.      , .false., 'yearly'  , ''       , ''       , ''
   sn_rnf2(2)  = 'GI_runoff_ISBA_noAA_noGR_clim_366' , 24.   , 'sorunoff', .true.      , .true. , 'yearly'  , ''       , ''       , ''
/

```

## Building `GI_runoff_ISBA_noAA_noGR_clim_366.nc`:
We took the global runoff climatology and apply a mask in order to keep only coastal runoff corresponding to Amazon-Orenoque, Niger-Congo areas.

See the [bash script](../BUILD/AI_RUNOFF/mk_clim_AI.sh) used to produce the GA and GI runoff input.

