# IMHOTEP.GAI
## Description
This experiment uses the exact same code than IMHOTEP.S. The same correction term is added to the precip and no SSS restoring is used.

## Continental Fresh Water Fluxes:
In this experiment, all continental fresh water fluxes have an interannual variability (Except for Antarctica where we only have climatological forcing).
### corresponding namelist :
  * River Runoff

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
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_rnf      = 'eORCA025_runoff_ISBA_noAA_noGR' , 24.      , 'sorunoff',   .true.    , .false. , 'yearly' ,  ''              , ''       , ''
   sn_cnf      = 'eORCA025.L75_rnf_msk'  ,         -12.      , 'socoefr' ,   .false.   , .true. , 'yearly'  ,  ''              , ''       , ''
   sn_s_rnf    = ' '                     ,          24.      , 'rosaline',   .true.    , .true. , 'yearly'  ,  ''              , ''       , ''
   sn_t_rnf    = ' '                     ,          24.      , 'rotemper',   .true.    , .true. , 'yearly'  ,  ''              , ''       , ''
   sn_dep_rnf  = 'eORCA025.L75_rnf_dep ' ,           0.      , 'sozisfmax' , .false.   , .true. , 'yearly'  ,  ''              , ''       , ''
   sn_i_rnf    = 'NOT_USED'              ,        -1.        , 'sorunoff',   .true.    , .true. , 'yearly'  ,  ''              , ''       , ''
/
!-----------------------------------------------------------------------
&namsbc_rnf_drk  !   runoffs  drakkar multiple file enhancement         (ln_rnf =T)
!-----------------------------------------------------------------------
   nn_rnf_freq = 2
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_rnf2(1)  = 'eORCA025.L75_1m_greenland_rnfbis' , -1.    , 'sorunoff', .true.      , .false. , 'yearly'  , ''              , ''       , ''
/
```

  * ISF runoff:

```fortran
!-----------------------------------------------------------------------
&namsbc_isf    !  Top boundary layer (ISF)                              (ln_isfcav =T : read (ln_read_cfg=T)
!-----------------------------------------------------------------------             or set or usr_def_zgr )
   !                 ! type of top boundary layer
   nn_isf      = 3         !  ice shelf melting/freezing
                           !  1 = presence of ISF   ;  2 = bg03 parametrisation
                           !  3 = rnf file for ISF  ;  4 = ISF specified freshwater flux
                           !  options 1 and 4 need ln_isfcav = .true. (domzgr)
      !              !  nn_isf = 1 or 2 cases:
      rn_gammat0  = 1.e-4     ! gammat coefficient used in blk formula
      rn_gammas0  = 1.e-4     ! gammas coefficient used in blk formula
      !              !  nn_isf = 1 or 4 cases:
      rn_hisf_tbl =  30.      ! thickness of the top boundary layer    (Losh et al. 2008)
      !                       ! 0 => thickness of the tbl = thickness of the first wet cell
      !              ! nn_isf = 1 case
      nn_isfblk   = 1         ! 1 ISOMIP  like: 2 equations formulation (Hunter et al., 2006)
      !                       ! 2 ISOMIP+ like: 3 equations formulation (Asay-Davis et al., 2015)
      nn_gammablk = 1         ! 0 = cst Gammat (= gammat/s)
      !                       ! 1 = velocity dependend Gamma (u* * gammat/s)  (Jenkins et al. 2010)
      !                       ! 2 = velocity and stability dependent Gamma    (Holland et al. 1999)

   !___________!_________________________!___________________!___________!_____________!_________!___________!__________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !  (logical)  !  (T/F)  ! 'monthly' ! filename ! pairing  ! filename      !
!* nn_isf = 3 case
   sn_rnfisf   = 'eORCA025_rnfisf_b0.2_c3.0_d1.0_v0.0', -12. ,'sofwfisf' ,  .false.    , .true.  , 'yearly'  ,    ''    ,   ''     ,    ''
!* nn_isf = 2 and 3 cases
   sn_depmax_isf ='eORCA025.L75_rnf_dep' ,        -12.       ,'sozisfmax',  .false.    , .true.  , 'yearly'  ,    ''    ,   ''     ,    ''
   sn_depmin_isf ='eORCA025.L75_rnf_dep' ,        -12.       ,'sozisfmin',  .false.    , .true.  , 'yearly'  ,    ''    ,   ''     ,    ''
/
!-----------------------------------------------------------------------
&namsbc_isf_drk    !  Top boundary layer (ISF) drakkar                   (ln_isfcav =T : read (ln_read_cfg=T) + key_drakkar
!-----------------------------------------------------------------------             or set or usr_def_zgr )
   nn_rnfisf_freq = 2

   !___________!_________________________!___________________!___________!_____________!_________!___________!__________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !  (logical)  !  (T/F)  ! 'monthly' ! filename ! pairing  ! filename      !
   sn_rnfisf2(1) = 'eORCA025.L75_1m_greenland_isfbis' , -1.  ,'sornfisf' ,  .true.     , .false. , 'yearly'  ,    ''    ,   ''     ,    ''
/
```

  * Calving:

```fortran
!-----------------------------------------------------------------------
&namberg       !   iceberg parameters                                   (default: OFF)
!-----------------------------------------------------------------------
   ln_icebergs = .true.      ! activate iceberg floats (force =F with "key_agrif")
   !
   !                          ! diagnostics:
   ln_bergdia        = .true.        ! Calculate budgets
   nn_verbose_level  = 1             ! Turn on more verbose output if level > 0
   nn_verbose_write  = 72            ! Timesteps between verbose messages
   nn_sample_rate    = 72            ! Timesteps between sampling for trajectory storage
   !
   !                          ! iceberg setting:
   !                                 ! Initial mass required for an iceberg of each class
   rn_initial_mass   = 8.8e7, 4.1e8, 3.3e9, 1.8e10, 3.8e10, 7.5e10, 1.2e11, 2.2e11, 3.9e11, 7.4e11
   !                                 ! Proportion of calving mass to apportion to each class
   rn_distribution   = 0.24, 0.12, 0.15, 0.18, 0.12, 0.07, 0.03, 0.03, 0.03, 0.02
   !                                 ! Ratio between effective and real iceberg mass (non-dim)
   !                                 ! i.e. number of icebergs represented at a point
   rn_mass_scaling   = 2000., 200., 50., 20., 10., 5., 2., 1., 1., 1.
                                     ! thickness of newly calved bergs (m)
   rn_initial_thickness     = 40., 67., 133., 175., 250., 250., 250., 250., 250., 250.
   !
   rn_rho_bergs            = 850.    ! Density of icebergs
   rn_LoW_ratio            = 1.5     ! Initial ratio L/W for newly calved icebergs
   ln_operator_splitting   = .true.  ! Use first order operator splitting for thermodynamics
   rn_bits_erosion_fraction = 0.     ! Fraction of erosion melt flux to divert to bergy bits
   rn_sicn_shift           = 0.      ! Shift of sea-ice concn in erosion flux (0<sicn_shift<1)
   ln_passive_mode         = .false. ! iceberg - ocean decoupling
   nn_test_icebergs        =  -1     ! Create test icebergs of this class (-1 = no)
   !                                 ! Put a test iceberg at each gridpoint in box (lon1,lon2,lat1,lat2)
   rn_test_box             = 108.0,  116.0, -66.0, -58.0
   ln_use_calving          = .true.  ! Use calving data even when nn_test_icebergs > 0
   rn_speed_limit          = 0.      ! CFL speed limit for a berg

   cn_dir      = './'      !  root directory for the calving data location
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_icb   =  'eORCA025_calving_b0.2_v2.3_ANTARCTIC' , -12. ,'soicbclv' , .false      , .true. , 'yearly'  , ''               , ''       , ''
/
!-----------------------------------------------------------------------
&namberg_drk      !   drakkar iceberg parameters
!-----------------------------------------------------------------------
      ln_rstart_icb            = .true.
      cn_icbrst_in             = "restart_icebergs"   ! name of restart file
      cn_icbrst_out            = "restart_icebergs"
      cn_icbdir_trj            = "<CN_DIRICB>"          ! name of directory for icb trajectory output
   nn_icb_freq = 2

   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_icb2(1) = 'eORCA025.L75_1m_greenland_calvingbis' , -1. ,'soicbclv' , .TRUE.      , .false., 'yearly'  , ''               , ''       , ''
/
```
