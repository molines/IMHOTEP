PROGRAM fdbk_reader_alt
  !!======================================================================
  !!                     ***  PROGRAM   fdbk_reader_alt ***
  !!=====================================================================
  !!  ** Purpose : read and display values (table) for fsbl altimeter files
  !!
  !!  ** Method  :  netcdf
  !!
  !! History :  1.0  : 07/2023  : J.M. Molines : 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------


  !!----------------------------------------------------------------------
  !! FDBK , MEOM 2023
  !!----------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER(KIND=4) :: jo
  INTEGER(KIND=4) :: narg, iargc, ijarg
  INTEGER(KIND=4) :: ncid,id, ierr
  INTEGER(KIND=4) :: id_lon,id_lat
  INTEGER(KIND=4) :: id_dep, id_dep_qc, id_dep_qc_fl
  INTEGER(KIND=4) :: id_jul, id_jul_qc, id_jul_qc_fl
  INTEGER(KIND=4) ::         id_obs_qc, id_obs_qc_fl
  INTEGER(KIND=4) ::         id_pos_qc, id_pos_qc_fl
  INTEGER(KIND=4) :: id_slaob, id_slaob_qc, id_slaob_qc_fl,id_slaob_lev_qc, id_slaob_lev_qc_fl
  INTEGER(KIND=4) :: id_slahx, id_slassh
  INTEGER(KIND=4) :: n_obs, n_levels, np
  INTEGER(KIND=4) :: itbeg, itend

  ! variables :
  INTEGER(KIND=4),DIMENSION(1) ::iv_dep_qc,iv_obs_qc, iv_pos_qc, iv_jul_qc, iv_slaob_qc
  INTEGER(KIND=4),DIMENSION(1) ::iv_slaob_lev_qc
  ! QC FLAGS
  INTEGER(KIND=4), DIMENSION(2) :: iv_dep_qc_fl, iv_obs_qc_fl, iv_pos_qc_fl, iv_jul_qc_fl
  INTEGER(KIND=4), DIMENSION(2) :: iv_slaob_qc_fl, iv_slaob_lev_qc_fl

  REAL(KIND=4),DIMENSION(1)   :: rv_slaob, rv_slahx, rv_slassh

  REAL(KIND=8),DIMENSION(1)   :: dv_lon, dv_lat, dv_dep, dv_jul
  

  CHARACTER(LEN=255) :: cf_fdbk
  CHARACTER(LEN=255) :: cldum

  LOGICAL :: l_slahx=.TRUE.

  !---------------------
  narg = iargc()

  IF ( narg == 0 ) THEN
     PRINT *,' usage : fdbk_reader_alt  -f FDBK-FILE [-ti START-STEP] [-te END-STEP]'
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       Read and display values in altimeter feedback files' 
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'       -f FDBK-file : name of fdbk file to process' 
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'       -ti START-STEP : specify initial time step to show [1]'
     PRINT *,'       -ti END-STEP : specify last time step to show [n_obs]'
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       none' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'        on standard output'
     PRINT *,'      '
     PRINT *,'     SEE ALSO :'
     PRINT *,'      ' 
     PRINT *,'      '
     STOP
  ENDIF

  ijarg = 1 
  itbeg=1
  itend=-1
  DO WHILE ( ijarg <= narg )
     CALL getarg(ijarg, cldum ) ; ijarg=ijarg+1
     SELECT CASE ( cldum )
     CASE ( '-f'   ) ; CALL getarg(ijarg, cf_fdbk ) ; ijarg=ijarg+1
     CASE ( '-ti'  ) ; CALL getarg(ijarg, cldum )   ; ijarg=ijarg+1 ; READ(cldum,*) itbeg
     CASE ( '-te'  ) ; CALL getarg(ijarg, cldum )   ; ijarg=ijarg+1 ; READ(cldum,*) itend
     CASE DEFAULT    ; PRINT *, ' ERROR : ', TRIM(cldum),' : unknown option.'; STOP 1
     END SELECT
  ENDDO

  ! Open FDBK file
  ierr = NF90_OPEN(cf_fdbk, NF90_NOWRITE, ncid)
  ! read relevant dimensions:
  ierr = NF90_INQ_DIMID(ncid,'N_OBS'   ,id) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=n_obs   )
  ierr = NF90_INQ_DIMID(ncid,'N_LEVELS',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=n_levels)

  PRINT *,' FILE       : ', TRIM(cf_fdbk)
  PRINT *,'   N_OBS    : ', n_obs
  PRINT *,'   N_LEVELS : ', n_levels
  IF ( itend == -1 ) itend = n_obs
  ! look for variables id
  ierr = NF90_INQ_VARID(ncid,'LONGITUDE'            ,id_lon)
  ierr = NF90_INQ_VARID(ncid,'LATITUDE'             ,id_lat)
  ierr = NF90_INQ_VARID(ncid,'DEPTH'                ,id_dep)
  ierr = NF90_INQ_VARID(ncid,'DEPTH_QC'             ,id_dep_qc)
  ierr = NF90_INQ_VARID(ncid,'DEPTH_QC_FLAGS'       ,id_dep_qc_fl)
  ierr = NF90_INQ_VARID(ncid,'JULD'                 ,id_jul)
  ierr = NF90_INQ_VARID(ncid,'OBSERVATION_QC'       ,id_obs_qc)
  ierr = NF90_INQ_VARID(ncid,'OBSERVATION_QC_FLAGS' ,id_obs_qc_fl)
  ierr = NF90_INQ_VARID(ncid,'POSITION_QC'          ,id_pos_qc)
  ierr = NF90_INQ_VARID(ncid,'POSITION_QC_FLAGS'    ,id_pos_qc_fl)
  ierr = NF90_INQ_VARID(ncid,'JULD_QC'              ,id_jul_qc)
  ierr = NF90_INQ_VARID(ncid,'JULD_QC_FLAGS'        ,id_jul_qc_fl)
  ierr = NF90_INQ_VARID(ncid,'SLA_OBS'              ,id_slaob)
  ierr = NF90_INQ_VARID(ncid,'SLA_Hx'               ,id_slahx)
   IF ( ierr /= NF90_NOERR) THEN
       l_slahx=.false.
   ENDIF
  ierr = NF90_INQ_VARID(ncid,'SLA_SSH'              ,id_slassh)
  ierr = NF90_INQ_VARID(ncid,'SLA_QC'               ,id_slaob_qc)
  ierr = NF90_INQ_VARID(ncid,'SLA_QC_FLAGS'         ,id_slaob_qc_fl)
  ierr = NF90_INQ_VARID(ncid,'SLA_LEVEL_QC'         ,id_slaob_lev_qc)
  ierr = NF90_INQ_VARID(ncid,'SLA_LEVEL_QC_FLAGS'   ,id_slaob_lev_qc_fl)

  ! READ and print :
  PRINT *,' obs   lon          lat      jul         sla   mod        ssh      qc '

  np=0
  print *, itbeg, itend 
  DO jo = itbeg, itend
    if (mod(jo,1000) == 0 ) print *,'JOBS=', jo
    if (mod(np,20) == 1 ) THEN
  PRINT *,' obs   lon          lat      jul         sla   mod        ssh      qc '
    endif
    ierr = NF90_GET_VAR(ncid,id_lon,dv_lon,            start=(/jo/), count=(/1/))
    if ( ierr /= NF90_NOERR ) PRINT * ,TRIM(NF90_STRERROR(ierr)), ' A'
    ierr = NF90_GET_VAR(ncid,id_lat,dv_lat,            start=(/jo/), count=(/1/))
    if ( ierr /= NF90_NOERR ) PRINT * ,TRIM(NF90_STRERROR(ierr)), ' B'
    ierr = NF90_GET_VAR(ncid,id_jul,dv_jul,            start=(/jo/), count=(/1/))
    if ( ierr /= NF90_NOERR ) PRINT * ,TRIM(NF90_STRERROR(ierr)), ' C'
    ierr = NF90_GET_VAR(ncid,id_slaob, rv_slaob,       start=(/1,jo/), count=(/1,1/))
    if ( ierr /= NF90_NOERR ) PRINT * ,TRIM(NF90_STRERROR(ierr)), ' D'
    ierr = NF90_GET_VAR(ncid,id_slahx, rv_slahx,       start=(/1,jo/), count=(/1,1/))
    if ( ierr /= NF90_NOERR ) PRINT * ,TRIM(NF90_STRERROR(ierr)), ' E'
    ierr = NF90_GET_VAR(ncid,id_slassh, rv_slassh,     start=(/1,jo/), count=(/1,1/))
    if ( ierr /= NF90_NOERR ) PRINT * ,TRIM(NF90_STRERROR(ierr)), ' F'
    ierr = NF90_GET_VAR(ncid,id_slaob_qc, iv_slaob_qc, start=(/jo/), count=(/1/))
    if ( ierr /= NF90_NOERR ) PRINT * ,TRIM(NF90_STRERROR(ierr)), ' G'

!   IF ( rv_slahx(1) < 20 ) THEN
!    if ( ABS (rv_slaob(1)) > 9 .OR. (abs(rv_slahx(1)) > 9 .AND. ABS(rv_slahx(1)) /= 99999 ) ) THEN
     if ( ABS (rv_slaob(1)) > 1  ) THEN
!      if ( ABS (rv_slaob(1)) > 9 .OR. (abs(rv_slahx(1)) > 9 ) ) THEN
        PRINT '(i9,2f9.3,2x,f9.3, 3f10.3, i4)', jo, dv_lon, dv_lat, dv_jul, rv_slaob, rv_slahx, rv_slassh, iv_slaob_qc
      np=np+1
  ENDIF
  ENDDO


END PROGRAM fdbk_reader_alt
