# NEMO implementation for multiple runoff, calving ... files
## 1. Context:
It is very often that input data for runoff or calving have frequency depending on the localisation: for instance, in IMOTHEP we use IsBA interannual daily runoff almost
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
