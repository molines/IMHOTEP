PROGRAM aviso_fdbk
  !!======================================================================
  !!                     ***  PROGRAM  aviso_fdbk  ***
  !!=====================================================================
  !!  ** Purpose : Convert nadir altimetric data from AVISO to
  !!               NEMO feddback (fdbk) format.
  !!
  !!  ** Method  : READ/WRITE ... 
  !!
  !! History :  1.0  : 06/2022  : J.M. Molines : 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------
  USE netcdf
  !!----------------------------------------------------------------------
  !! DATATOOLS , MEOM 2022
  !! Copyright (c) 2012, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/CDFTOOLSCeCILL.txt)
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER(KIND=4), PARAMETER :: jp_asla=1, jp_alon=2, jp_alat=3
  INTEGER(KIND=4) :: jt

  INTEGER(KIND=4) :: ncfdbk
  INTEGER(KIND=4) :: nid_lon, nid_lat, nid_dep, nid_depqc, nid_depqcflag
  INTEGER(KIND=4) :: nid_juld, nid_obsqc,nid_obsqcflag, nid_posqc, nid_posqcflag
  INTEGER(KIND=4) :: nid_juldqc, nid_juldqcflag, nid_orifilind, nid_sla
  INTEGER(KIND=4) :: nid_slaqc, nid_slaqcflag, nid_slalevqc, nid_slalevqcflag
  INTEGER(KIND=4) :: nid_statid, nid_statyp, ierr

  INTEGER(KIND=4) :: ncaviso, nid_alon, nid_alat, nid_asla, nid_ajul
  INTEGER(KIND=4) :: ntimes
  INTEGER(KIND=4) :: narg, iargc, ijarg

  REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: dsla, dlon, dlat, dtmp, djul
  REAL(KIND=8), DIMENSION(3)              :: dadd_offset, dscale_factor

  CHARACTER(LEN=255) :: cf_aviso
  CHARACTER(LEN=255) :: cf_fdbk
  CHARACTER(LEN=4)   :: cs_type
  CHARACTER(LEN=255) :: cldum

  LOGICAL            :: lerr = .FALSE.
  !!----------------------------------------------------------------------

  narg=iargc()
  IF ( narg == 0 ) THEN
    PRINT *, 'USAGE : aviso_fdbk -f AVISO-file -sat SAT-type'
    PRINT *, '    Purpose:'
    PRINT *, '       Convert the input SLA aviso file into fdbk format'
    PRINT *, '    Options:'
    PRINT *, '       -f AVISO-file : give the name of SLA aviso file'
    PRINT *, '       -sat SAT-type : a 4 character max variable giving'
    PRINT *, '             a code for the satelite. e.g: tp, j1,j2,j3...'
    PRINT *, '             This follows the CMEMS nomenclature.'
    PRINT *, '    Output:'
    PRINT *, '       Output filename will be ''fdbk_''<AVISO-file>'
    PRINT *, '    References :'
    PRINT *, '       https://marine.copernicus.eu/ for original aviso'
    PRINT *, '       data set'
    STOP
  ENDIF

   cs_type='none'
   cf_aviso='none'
   ijarg=1
   DO WHILE (ijarg <= narg )
      CALL getarg(ijarg, cldum) ; ijarg=ijarg+1
      SELECT CASE (cldum)
      CASE ('-f'   ) ; CALL getarg(ijarg, cf_aviso) ; ijarg=ijarg+1
      CASE ('-sat' ) ; CALL getarg(ijarg, cs_type ) ; ijarg=ijarg+1
      CASE DEFAULT 
           PRINT *,' ERROR : unknown option : ',TRIM(cldum)
           STOP
      END SELECT
   END DO

   IF ( cf_aviso == 'none' ) lerr=.true.
   IF ( cs_type  == 'none' ) lerr=.true.

   IF ( lerr ) THEN
         PRINT *, ' ERROR : you must use -f AVISO-file -sat SAT-type'
         STOP
   ENDIF
   
  cf_fdbk='fdbk_'//TRIM(cf_aviso)

  CALL OpenAviso(cf_aviso)
  PRINT *, ' NTIMES = ', ntimes

  CALL CreateFDBK(cf_fdbk)

  ! aviso longitude 
  ierr = NF90_GET_VAR(ncaviso, nid_alon, dlon )
  dtmp=dlon*dscale_factor(jp_alon) + dadd_offset(jp_alon)
  dlon=dtmp

  ! aviso latitude
  ierr = NF90_GET_VAR(ncaviso, nid_alat, dlat )
  dtmp=dlat*dscale_factor(jp_alat) + dadd_offset(jp_alat)
  dlat=dtmp

  ! aviso sla
  ierr = NF90_GET_VAR(ncaviso, nid_asla, dsla )
  dtmp=dsla*dscale_factor(jp_asla) + dadd_offset(jp_asla)
  dsla=dtmp

  ! aviso time
  ierr = NF90_GET_VAR(ncaviso, nid_ajul, djul )


  ! write on cf_fdbk file
  ierr = NF90_PUT_VAR(ncfdbk,nid_lon, dlon, start=(/1/)  , count=(/ntimes/) )
  ierr = NF90_PUT_VAR(ncfdbk,nid_lat, dlat, start=(/1/)  , count=(/ntimes/) )
  ierr = NF90_PUT_VAR(ncfdbk,nid_sla, dsla, start=(/1,1/), count=(/1,ntimes/) )
  ierr = NF90_PUT_VAR(ncfdbk,nid_juld, djul, start=(/1/) , count=(/ntimes/) )
   
  ierr = NF90_CLOSE(ncfdbk)

  CONTAINS

  SUBROUTINE CreateFDBK(cd_fdbk)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CreateFDBK  ***
    !!
    !! ** Purpose :  Create and initialise FeedBack file  
    !!
    !! ** Method  :  Use NEtcdf primitives 
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_fdbk
    !!
    INTEGER(KIND=4) :: ierr
    INTEGER(KIND=4) :: id_nobs,id_nlevels,id_nvars,id_nqcf
    INTEGER(KIND=4) :: id_stringnam, id_stringwmo, id_stringtyp,id_stringjuld
    INTEGER(KIND=4) :: id_var, id_julref
    INTEGER(KIND=4),  DIMENSION(1) :: idum

    REAL(KIND=8),     DIMENSION(1) :: dl_dep
    CHARACTER(LEN=8), DIMENSION(1) :: cl_varnm
    CHARACTER(LEN=14) :: cl_julref
    !! ---------------------------------------------------------------------
!   ierr = NF90_CREATE(cd_fdbk, NF90_NETCDF4, ncfdbk )
    ierr = NF90_CREATE(cd_fdbk, NF90_64BIT_OFFSET, ncfdbk )
    ! define dimensions
    ierr = NF90_DEF_DIM(ncfdbk,'N_OBS'     ,NF90_UNLIMITED,id_nobs      )
    ierr = NF90_DEF_DIM(ncfdbk,'N_LEVELS'  ,1             ,id_nlevels   )
    ierr = NF90_DEF_DIM(ncfdbk,'N_VARS'    ,1             ,id_nvars     )
    ierr = NF90_DEF_DIM(ncfdbk,'N_QCF'     ,2             ,id_nqcf      )
    ierr = NF90_DEF_DIM(ncfdbk,'STRINGNAM' ,8             ,id_stringnam )
    ierr = NF90_DEF_DIM(ncfdbk,'STRINGWMO' ,8             ,id_stringwmo )
    ierr = NF90_DEF_DIM(ncfdbk,'STRINGTYP' ,4             ,id_stringtyp )
    ierr = NF90_DEF_DIM(ncfdbk,'STRINGJULD',14            ,id_stringjuld)
    ! define variables
    ierr = NF90_DEF_VAR(ncfdbk,'LONGITUDE'            ,NF90_DOUBLE,(/id_nobs/)                   ,nid_lon          )
    ierr = NF90_DEF_VAR(ncfdbk,'LATITUDE'             ,NF90_DOUBLE,(/id_nobs/)                   ,nid_lat          )
    ierr = NF90_DEF_VAR(ncfdbk,'VARIABLES'            ,NF90_CHAR,  (/id_stringnam,id_nvars/)     ,id_var           )
    ierr = NF90_DEF_VAR(ncfdbk,'STATION_IDENTIFIER'   ,NF90_CHAR,  (/id_stringwmo,id_nobs/)      ,nid_statid       )
    ierr = NF90_DEF_VAR(ncfdbk,'STATION_TYPE'         ,NF90_CHAR,  (/id_stringtyp,id_nobs/)      ,nid_statyp       )
    ierr = NF90_DEF_VAR(ncfdbk,'DEPTH'                ,NF90_DOUBLE,(/id_nlevels,id_nobs/)        ,nid_dep          )
    ierr = NF90_DEF_VAR(ncfdbk,'DEPTH_QC'             ,NF90_INT   ,(/id_nlevels,id_nobs/)        ,nid_depqc        )
    ierr = NF90_DEF_VAR(ncfdbk,'DEPTH_QC_FLAGS'       ,NF90_INT   ,(/id_nqcf,id_nlevels,id_nobs/),nid_depqcflag    )
    ierr = NF90_DEF_VAR(ncfdbk,'JULD'                 ,NF90_DOUBLE,(/id_nobs/)                   ,nid_juld         )
    ierr = NF90_DEF_VAR(ncfdbk,'JULD_REFERENCE'       ,NF90_CHAR  ,(/id_stringjuld/)             ,id_julref        )
    ierr = NF90_DEF_VAR(ncfdbk,'OBSERVATION_QC'       ,NF90_INT   ,(/id_nobs/)                   ,nid_obsqc        )
    ierr = NF90_DEF_VAR(ncfdbk,'OBSERVATION_QC_FLAGS' ,NF90_INT   ,(/id_nqcf,id_nobs/)           ,nid_obsqcflag    )
    ierr = NF90_DEF_VAR(ncfdbk,'POSITION_QC'          ,NF90_INT,   (/id_nobs/)                   ,nid_posqc        )
    ierr = NF90_DEF_VAR(ncfdbk,'POSITION_QC_FLAGS'    ,NF90_INT   ,(/id_nqcf,id_nobs/)           ,nid_posqcflag    )
    ierr = NF90_DEF_VAR(ncfdbk,'JULD_QC'              ,NF90_INT   ,(/id_nobs/)                   ,nid_juldqc       )
    ierr = NF90_DEF_VAR(ncfdbk,'JULD_QC_FLAGS'        ,NF90_INT   ,(/id_nqcf,id_nobs/)           ,nid_juldqcflag   )
    ierr = NF90_DEF_VAR(ncfdbk,'ORIGINAL_FILE_INDEX'  ,NF90_INT   ,(/id_nobs/)                   ,nid_orifilind    )
    ierr = NF90_DEF_VAR(ncfdbk,'SLA_OBS'              ,NF90_FLOAT ,(/id_nlevels,id_nobs/)        ,nid_sla          ) 
    ierr = NF90_DEF_VAR(ncfdbk,'SLA_QC'               ,NF90_INT   ,(/id_nobs/)                   ,nid_slaqc        )
    ierr = NF90_DEF_VAR(ncfdbk,'SLA_QC_FLAGS'         ,NF90_INT   ,(/id_nqcf,id_nobs/)           ,nid_slaqcflag    )
    ierr = NF90_DEF_VAR(ncfdbk,'SLA_LEVEL_QC'         ,NF90_INT   ,(/id_nlevels,id_nobs/)        ,nid_slalevqc     )
    ierr = NF90_DEF_VAR(ncfdbk,'SLA_LEVEL_QC_FLAGS'   ,NF90_INT   ,(/id_nqcf,id_nlevels,id_nobs/),nid_slalevqcflag )
    ! define attribute
    ierr = NF90_PUT_ATT(ncfdbk, nid_lon,'_FillValue'    ,99999.d0 )
    ierr = NF90_PUT_ATT(ncfdbk, nid_lon,'long_name','Longitude' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_lon,'units' ,'degrees_east' )

    ierr = NF90_PUT_ATT(ncfdbk, nid_lat,'_FillValue'    ,99999.d0 )
    ierr = NF90_PUT_ATT(ncfdbk, nid_lat,'long_name','Latitude' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_lat,'units' ,'degrees_north' )

    ierr = NF90_PUT_ATT(ncfdbk, id_var,'long_name','List of variables in feedback files' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_statid,'long_name','Station identifier' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_statyp,'long_name','Code instrument type')

    ierr = NF90_PUT_ATT(ncfdbk, nid_dep,'_FillValue'    ,99999.d0 )
    ierr = NF90_PUT_ATT(ncfdbk, nid_dep,'long_name','Depth' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_dep,'units' ,'meters' )

    ierr = NF90_PUT_ATT(ncfdbk, nid_depqc,'long_name','Quality on depth' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_depqc,'Conventions','q where q =[0,9]' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_depqc,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_depqcflag,'long_name','Quality flags on depth' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_depqcflag,'Conventions','NEMOVAR flag conventions' )

    ierr = NF90_PUT_ATT(ncfdbk, nid_juld,'long_name','Julian day' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_juld,'units' ,'days since JULD_REFERENCE' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_juld,'Conventions' ,'relative julian days with decimal part (as parts of day)' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_juld,'_FillValue'    ,99999.d0 )

    ierr = NF90_PUT_ATT(ncfdbk, id_julref,'long_name','Date of reference for julian days' )
    ierr = NF90_PUT_ATT(ncfdbk, id_julref,'Conventions' ,'YYYYMMDDHHMMSS' )

    ierr = NF90_PUT_ATT(ncfdbk, nid_obsqc,'long_name','Quality on observation' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_obsqc,'Conventions','q where q =[0,9]' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_obsqc,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_obsqcflag,'long_name','Quality flags on observation' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_obsqcflag,'Conventions','NEMOVAR flag conventions' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_obsqcflag,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_posqc,'long_name','Quality on position (latitude and longitude)' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_posqc,'Conventions','q where q =[0,9]' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_posqc,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_posqcflag,'long_name','Quality flags on position' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_posqcflag,'Conventions','NEMOVAR flag conventions' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_posqcflag,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_juldqc,'long_name','Quality on date and time' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_juldqc,'Conventions','q where q =[0,9]' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_juldqc,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_juldqcflag,'long_name','Quality flags on date and time' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_juldqcflag,'Conventions','NEMOVAR flag conventions' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_juldqcflag,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_orifilind,'long_name','Index in original data file' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_orifilind,'_Fillvalue',-99999 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_sla,'long_name','SLA observations' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_sla,'units' ,'meters' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_sla,'_FillValue'    ,99999. )

    ierr = NF90_PUT_ATT(ncfdbk, nid_slaqc,'long_name','Quality on SLA observations')
    ierr = NF90_PUT_ATT(ncfdbk, nid_slaqc,'Conventions','q where q =[0,9]' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_slaqc,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_slaqcflag,'long_name','Quality flags on SLA observations')
    ierr = NF90_PUT_ATT(ncfdbk, nid_slaqcflag,'Conventions','NEMOVAR flag conventions' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_slaqcflag,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_slalevqc,'long_name','Quality for each level on SLA observations' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_slalevqc,'Conventions','q where q =[0,9]' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_slalevqc,'_Fillvalue',0 )

    ierr = NF90_PUT_ATT(ncfdbk, nid_slalevqcflag,'long_name','Quality flags for each level on SLA observations' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_slalevqcflag,'Conventions','NEMOVAR flag conventions' )
    ierr = NF90_PUT_ATT(ncfdbk, nid_slalevqcflag,'_Fillvalue',0 )
    ! END DEFINITION 
    ierr = NF90_ENDDEF(ncfdbk)
    
    ! fill static variables once for all (id_var, od_julref
    cl_varnm(1)='SLA     '
    cl_julref='19500101000000'
    ierr = NF90_PUT_VAR(ncfdbk, id_var   ,cl_varnm(:)(1:8) )
    ierr = NF90_PUT_VAR(ncfdbk, id_julref,cl_julref(1:14),start =(/1/), count=(/14/))
    ! fill dummy variable (so much !)
    dl_dep(1) = 0.d0
    DO jt = 1, ntimes
     ierr = NF90_PUT_VAR(ncfdbk,nid_statid,cs_type(1:4), start=(/1,jt/), count=(/4,1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_statyp,' 7'        , start=(/1,jt/), count=(/4,1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_dep,    dl_dep     , start=(/1,jt/), count=(/1,1/) )
     idum = 0
     ierr = NF90_PUT_VAR(ncfdbk,nid_depqc, idum        , start=(/1,jt/), count=(/1,1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_depqcflag, idum    , start=(/1,1,jt/), count=(/1,1,1/) )
     idum = -99999
     ierr = NF90_PUT_VAR(ncfdbk,nid_depqcflag, idum    , start=(/2,1,jt/), count=(/1,1,1/) )
     idum = 0
     ierr = NF90_PUT_VAR(ncfdbk,nid_obsqc, idum        , start=(/jt/)  , count=(/1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_obsqcflag, idum    , start=(/1,jt/), count=(/1,1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_obsqcflag, idum    , start=(/2,jt/), count=(/1,1/) )
     idum = 0
     ierr = NF90_PUT_VAR(ncfdbk,nid_posqc, idum        , start=(/jt/)  , count=(/1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_posqcflag, idum    , start=(/1,jt/), count=(/1,1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_posqcflag, idum    , start=(/2,jt/), count=(/1,1/) )
     idum = 0
     ierr = NF90_PUT_VAR(ncfdbk,nid_juldqc, idum       , start=(/jt/)  , count=(/1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_juldqcflag, idum   , start=(/1,jt/), count=(/1,1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_juldqcflag, idum   , start=(/2,jt/), count=(/1,1/) )
     idum = 99999
     ierr = NF90_PUT_VAR(ncfdbk,nid_orifilind, idum    , start=(/jt/)  , count=(/1/) )
     idum = 1
     ierr = NF90_PUT_VAR(ncfdbk,nid_slaqc, idum       , start=(/jt/)  , count=(/1/) )
     idum = 0
     ierr = NF90_PUT_VAR(ncfdbk,nid_slaqcflag, idum   , start=(/1,jt/), count=(/1,1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_slaqcflag, idum   , start=(/2,jt/), count=(/1,1/) )
     idum = 1
     ierr = NF90_PUT_VAR(ncfdbk,nid_slalevqc, idum       , start=(/1,jt/)  , count=(/1,1/) )
     idum = 0
     ierr = NF90_PUT_VAR(ncfdbk,nid_slalevqcflag, idum   , start=(/1,1,jt/), count=(/1,1,1/) )
     ierr = NF90_PUT_VAR(ncfdbk,nid_slalevqcflag, idum   , start=(/2,1,jt/), count=(/1,1,1/) )
    ENDDO
    
  END SUBROUTINE CreateFDBK

  SUBROUTINE OpenAviso(cd_aviso)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE  OpenAviso ***
    !!
    !! ** Purpose :   Open the Aviso file and read dimensions
    !!                look for id of relevant variables
    !!                save add_offset and scale factor for
    !!                relevant variables
    !!
    !! ** Method  :   netcdf primitives
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*),  INTENT(in) :: cd_aviso
   
    INTEGER(KIND=4) :: ierr, id
    !!----------------------------------------------------------------------
    ierr = NF90_OPEN(cd_aviso, NF90_NOWRITE, ncaviso )
    IF ( ierr /= NF90_NOERR ) THEN
       PRINT *, ' ERROR : ', TRIM(cd_aviso)//' :  '//TRIM(NF90_STRERROR(ierr))
       STOP
    ENDIF
    ! get dimension
    ierr = NF90_INQ_DIMID(ncaviso,'time', id) ; ierr = NF90_INQUIRE_DIMENSION(ncaviso,id,len=ntimes)
    ALLOCATE( dsla(ntimes), dlon(ntimes), dlat(ntimes), djul(ntimes), dtmp(ntimes) )

    ! look for varid
    ierr = NF90_INQ_VARID(ncaviso,'sla_filtered ', nid_asla )
    ierr = NF90_INQ_VARID(ncaviso,'longitude'    , nid_alon )
    ierr = NF90_INQ_VARID(ncaviso,'latitude '    , nid_alat )
    ierr = NF90_INQ_VARID(ncaviso,'time'         , nid_ajul )

    ! get add off set and scale factor
    ierr = NF90_GET_ATT(ncaviso, nid_asla,'add_offset'  ,dadd_offset(jp_asla)   )
    ierr = NF90_GET_ATT(ncaviso, nid_asla,'scale_factor',dscale_factor(jp_asla) )

    ierr = NF90_GET_ATT(ncaviso, nid_alon,'add_offset'  ,dadd_offset(jp_alon)   )
    ierr = NF90_GET_ATT(ncaviso, nid_alon,'scale_factor',dscale_factor(jp_alon) )

    ierr = NF90_GET_ATT(ncaviso, nid_alat,'add_offset'  ,dadd_offset(jp_alat)   )
    ierr = NF90_GET_ATT(ncaviso, nid_alat,'scale_factor',dscale_factor(jp_alat) )


print *, dadd_offset
print *, dscale_factor


  END SUBROUTINE OpenAviso
END PROGRAM aviso_fdbk
