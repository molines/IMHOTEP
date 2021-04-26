# NEMO implementation for multiple runoff, calving ... files
## 1. Context:
It is very often that input data for runoff or calving have frequency depending on the localisation: for instance, in IMHOTEP we use IsBA interannual daily runoff almost
everywhere, except around Greenland where data are only monthly. On the other hand, in some part of the project, we will switch off interannual runoff locally and use climatology
instead.  Therefore there is a need for using multiple files for runoff or calving or even ISF, having different frequency or different type (climatology vs interannual). 

## 2. Road Map:
In NEMO, information about the data set used for forcing is often passed to the code through the `fldread` process. In the namelist, this corresponds to the `sn_`structure
where filename and variable names are defined, where frequency and shape of the data (yearly or monthly) are indicated, as well as the climatological or interannual content of the files.  Also, in case of interpolation on the fly, a weight file can be associated with the data file (when it is not on the model grid). At the end, in NEMO, this namelist 
information is used to build a new structure (`sf_xxx`) containing the data and all the relevant information. At a given time-step, the forcing field is available in the array
`sf_xxx(index)%now(:,:,1)`. Note that index is in general 1 for forcing data; it is used for tracer data where it takes the value jptem or jpsal (for instance in
`sf_tsd(jptem)%now(:,:,:)` giving a 3D interpolation of the temperature field associated with `sf_tsd`.

For our present purpose, we will use index as a counter in case of various files (with different charatecteristics) used for the same field, sf_xxx(1) pointing to the
original and unique file in standard NEMO.

## 3. Calving
At the namelist level, we define (example for pedagogic purpose) :

```fortran
!-----------------------------------------------------------------------
&namberg_drk      !   drakkar iceberg parameters
!-----------------------------------------------------------------------
....
   nn_icb_freq = 3           ! total number of dataset associated to icb
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_icb2(1)  = 'eORCA025_calving_Greenland' , -1.          ,'soicbclv' ,  .TRUE.     , .true. , 'yearly'  , ''               , ''       , ''
   sn_icb2(2)  = 'eORCA025_calving_Svalblard' , -12.         ,'soicbclv' ,  .TRUE.     , .false. , 'yearly'  , ''              , ''       , ''
/
```

`nn_icb_freq` define the total number of frequencies used for icb (in a more general sense, the number of data set), including the standard one, defined in the standard
NEMO namelist block `namberg`.  Here 3, means 1 standard + 2 extra. In the example we have sn_icb(1) (first extra) corresponding to monthly climatological data, and
sn_icb(2) corresponding to an annual interannual data set.  Up to now, the maximum number of extra data set is limited to 5 (hard coded dimension), but can be increased to any
number is necessary.

At NEMO level, the routines affected by this new capability are :
  * **icbini.F90** : 

    ```fortran
    #if defined key_drakkar
         INTEGER, PUBLIC                                    ::   nn_icb_freq     !: Total number of frequency file for calving including standard
         TYPE(FLD_N), DIMENSION(5)                          ::   sn_icb2         !: extra frequency calving file ( less than 6 files do far)
         TYPE(FLD_N), DIMENSION(6)                          ::   slf_icb         !: local namelist structure to collect all different data set for ICB
    #endif
    ```
    
    As seen in the example nn_icb_freq, and sn_icb2 are part of the namelist block `namberg_drk`

    ```fortran
    #if defined key_drakkar
         CHARACTER(LEN=20) :: cl_no
         NAMELIST/namberg_drk/ cn_icbrst_in,  cn_icbrst_out, cn_icbdir_trj, nn_icb_freq, sn_icb2
    #endif
    ```

    Then when calling fld_fill (initialization of the sf_icb structure) we have :

    ```fortran
    #if defined key_drakkar
            ALLOCATE( sf_icb(nn_icb_freq), STAT=istat1 )         ! Create sf_icb structure (calving)
            DO ji = 1, nn_icb_freq
               ALLOCATE( sf_icb(ji)%fnow(jpi,jpj,1), STAT=istat2 )
               istat1 = istat1 + istat2
               ALLOCATE( sf_icb(ji)%fdta(jpi,jpj,1,2), STAT=istat3 )
               istat1 = istat1 + istat3
            ENDDO
            IF( istat1 > 0 ) THEN
               CALL ctl_stop( 'sbc_icb: unable to allocate sf_icb structure' )   ;   RETURN
            ENDIF
            !
            slf_icb(1) = sn_icb
            DO ji = 2, nn_icb_freq
               slf_icb(ji) = sn_icb2(ji-1)
            ENDDO
            !                                          ! fill sf_icb with the namelist (sn_icb) and control print
            CALL fld_fill( sf_icb, slf_icb(1:nn_icb_freq), cn_dir, 'icb_init', 'read calving data', 'namicb' )
    #else
      .... standard NEMO code
    ```
  * **icbstp.F90**:
    When using sf_icb()%now we have the following modifications:

    ```fortran
          IF( nn_test_icebergs < 0 .OR. ln_use_calving ) THEN !* read calving data
             !
             CALL fld_read ( kt, 1, sf_icb )
    #if defined key_drakkar
             src_calving(:,:) = 0._wp    ! need to restore to 0 because of the accumulation
             DO ji = 1, nn_icb_freq  ! Loop on data set
               src_calving     (:,:) = src_calving(:,:)+ sf_icb(ji)%fnow(:,:,1)    ! calving in km^3/year (water equivalent)
             ENDDO
             src_calving_hflx(:,:) = 0._wp                    ! NO heat flux for now
    #else
             src_calving     (:,:) = sf_icb(1)%fnow(:,:,1)    ! calving in km^3/year (water equivalent)
             src_calving_hflx(:,:) = 0._wp                    ! NO heat flux for now
    #endif
            !
         ENDIF
    ```

## 4. Runoff
### 4.1 Analysis:
For runoff data set, we can consider the same technique in order to deal with various runoff dataset.  However, it is a bit more
complex as for liquid runoff, there are already many different `sn_` structures in addition to the basic one (`sn_rnf`). In fact, we have the
possible following data set, associated with liquid runoff (taken from an example namelist):

```fortran
!-----------------------------------------------------------------------
&namsbc_rnf    !   runoffs                                              (ln_rnf =T)
!-----------------------------------------------------------------------
   ln_rnf_mouth  = .true.     !  specific treatment at rivers mouths
      rn_hrnf    =  10.e0     !  depth over which enhanced vertical mixing is used    (ln_rnf_mouth=T)
      rn_avt_rnf =   2.e-3    !  value of the additional vertical mixing coef. [m2/s] (ln_rnf_mouth=T)
   rn_rfact      =   1.e0     !  multiplicative factor for runoff

   ! mandatory runoff information
   cn_dir      = './'      !  root directory for the runoff data location
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_rnf      = 'eORCA025_runoff_ISBA_clim_0.0' , -1.       , 'sorunoff', .true.      , .true. , 'yearly'  , ''       , '' , ''
   sn_cnf      = 'eORCA025_runoff_ISBA_clim_0.0' , -12.      , 'socoefr' , .false.     , .true. , 'yearly'  , ''       , '' , ''
   ! optional runoff information
   ln_rnf_depth  = .false.    !  read in depth information for runoff
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_dep_rnf  = ' '            ,         0.       , 'rodepth' ,   .false.    , .true. , 'yearly'  , ''       , '' , ''

   ln_rnf_tem    = .false.    !  read in temperature information for runoff
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_t_rnf    = ' '            ,        24.       , 'rotemper',   .true.     , .true. , 'yearly'  , ''       , '' , ''
   ln_rnf_sal    = .false.    !  read in salinity information for runoff
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_s_rnf    = ' '            ,        24.       , 'rosaline',   .true.     , .true. , 'yearly'  , ''       , '' , ''

   ln_rnf_depth_ini = .false. ! compute depth at initialisation from runoff file
      rn_rnf_max  = 5.735e-4  !  max value of the runoff climatologie over global domain ( ln_rnf_depth_ini = .true )
      rn_dep_max  = 150.      !  depth over which runoffs is spread ( ln_rnf_depth_ini = .true )
      nn_rnf_depth_file = 0   !  create (=1) a runoff depth file or not (=0)

   !  iceberg put as runoff 
   ln_rnf_icb  = .false.   !  read in iceberg flux from a file (fill sn_i_rnf if .true.)
   !___________!_________________________!___________________!___________!_____________!________!___________!__________________!__________!_______________!
   !           !  file name              ! frequency (hours) ! variable  ! time interp.!  clim  ! 'yearly'/ ! weights filename ! rotation ! land/sea mask !
   !           !                         !  (if <0  months)  !   name    !   (logical) !  (T/F) ! 'monthly' !                  ! pairing  !    filename   !
   sn_i_rnf    = 'NOT_USED'              ,        -1.        , 'sorunoff',   .true.    , .true. , 'yearly'  , ''               , ''       , ''

/
```

Up to now, we do not plan to use neither temperatures nor salinities for runoff. So we can pragmatically focus on the other fields:
  * **sn_rnf**: This is the river discharge data set and obviously there can be multiple files (different frequency or climatologies vs interannual).
  * **sn_cnf**: This field is a runoff mask, indicating to NEMO where the runoff are applied. This information was historically used for many purposes but
primarily for changing the tracer advection scheme for stability reason. (The centered 2nd order tracer advection scheme was the default scheme, and at
runoff points, it was necessary to use an upstream scheme, or at least a mix of the two schemes). Now that we almost always use the FCT --ex TVD-- tracer
advection scheme, the runoff mask is not more used for this purpose. But indeed, it is used for switching-off the SSS restoring at the runoff points.  In 
the DRAKKAR version of NEMO, we have implemented the option of fading out the SSS-restoring in coastal areas, on the base of a dataset giving the distance
to the coast in the ocean.  Therefore, it is likely that only one file is enough to describe all the runoff points, independently from the frequency or
characteristics (climatological or interannual) of the runoffs.
  * **sn_dep_rnf**:  This field gives the deptht of the water column on which the runoff is applied.  Using or not this information is an option and we decided to 
use it instead of adding an extra vertical mixing at runoff points. 
    * There are no sf_dep_rnf structure as this is a constant field in time, therefore the namelist entry is used only to get the file name and variable name.
    * The question of multiple depth is also raised:  Is it possible to have different depth for a single point ? **Need to check** *
       * ==> When using VVL, the depth of the runoff plays a role when updating the horizontal divergence on the layers impacted by the runoff.  Having multiple depth 
requires having multiple nk_rnf, rnf and rnf_b arrays ! (h_rnf are recomputed from nk_rnf, using updated values of e3t_n).  Note also that rnf_b array is written in
restart files and in case of multiple rnf_b this has to be changed too. 

       > As a conclusion on this point : using multiple depth is feasable but quite intrusive. If for a given model grid point, there are many runoff contributions with different 
depths, (and I have check up to 6 contributions for a single point), there should be as many files as contributions. 

> A minimum work around  can be to sum up all water discharge contributing to a single point, and to take the maximum runoff depth of the contribution as the actual
runoff depth.  This will be done in the preparation of the file, and the change in NEMO in this case will be very similar to the calving case, playing with multiple
dataset.

> The question of using ISF param for the glacier runoff around greenland is raised. The difference being in the latent heat flux corresponding to icemelt, in case of
the ISF parameterization. ISF parametrization take a range of depths corresponding the the depths of the grounding line (max) and to the depth of the ice at the 
iceshelf edge (min). **UPDATE:** choice was made (after discussion with P. Mathiot) to put only the contribution of the iceberg melting into the fjords into the ISF parametrization

### 4.2 Implementation in NEMO
As a starting point, we choose to add multiple files capability only for the runoff dataset, and this is primarily done for having different frequencies and data type 
(climatology vs interannual). We thus assume that runoff mask and runoff depths are described in separate single files.

For such an implementation, the modification are very similar to the modification performed for calving, and the impacted routines are:
