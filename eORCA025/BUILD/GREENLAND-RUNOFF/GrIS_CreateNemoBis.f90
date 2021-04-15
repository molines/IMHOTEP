PROGRAM GrIS_CreateNemoBis
  !!======================================================================
  !!                     ***  PROGRAM  GrIS_CreateNemoBis  ***
  !!=====================================================================
  !!  ** Purpose : Read J.Mouginot GrIS file and produce the set of
  !!               file for NEMO; splitted in Calving, Runoff/dep_rnf, ISF
  !!               This Bis version, tries to separate part the liquid water
  !!               coming from ice melting in the sea and liquid water coming
  !!               from inland melting.
  !!
  !!  ** Method  : netcdf
  !!
  !! History :  1.0  : 04/2021  : J.M. Molines : 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------

  USE netcdf

  !!----------------------------------------------------------------------
  !! CDFTOOLS_4.0 , MEOM 2017
  !! $Id$
  !! Copyright (c) 2012, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/CDFTOOLSCeCILL.txt)
  !!----------------------------------------------------------------------
  IMPLICIT NONE
  ! General dummy variables
  INTEGER(KIND=4) :: ii, ij, ik, ipb
  INTEGER(KIND=4) :: ji, jj,jt, jj1

  REAL(KIND=8)    :: dconv  !: conversion factor
  REAL(KIND=8)    :: darea  !: model cell surface

  ! GrIS file related
  INTEGER(KIND=4) :: npoints, nptim
  INTEGER(KIND=4) :: iibase, ijbase
  INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: nii, nij, ndouble

  INTEGER(KIND=8), DIMENSION(:), ALLOCATABLE :: nitime

  REAL(KIND=8),    DIMENSION(:),   ALLOCATABLE :: dlon, dlat, drnfdep
  REAL(KIND=8),    DIMENSION(:,:), ALLOCATABLE :: drnf, dcalv, dlamt, dphit

  CHARACTER(LEN=255) :: cf_GrIS='eORCA025_GrIS_forcing_50percent_solid.nc'

  LOGICAL,         DIMENSION(:),   ALLOCATABLE :: ldouble

  ! NEMO file related
  INTEGER(KIND=4) :: npiglo, npjglo

  REAL(KIND=4),    DIMENSION(:,:), ALLOCATABLE :: glamt, gphit, e1t, e2t
  REAL(KIND=4),    DIMENSION(:,:), ALLOCATABLE :: rnf, rnfmask, rnfisf, hmin, hmax, rcalv

  CHARACTER(LEN=255) :: cf_msh='eORCA025.L75_mesh_mask.nc'

  ! Parser related
  INTEGER(KIND=4) :: narg, ijarg, iargc
  CHARACTER(LEN=255) :: cldum

  ! Output Related
  INTEGER(KIND=4)    :: ierr
  CHARACTER(LEN=255) :: cf_rnf='GrIS_rnf.nc'
  INTEGER(KIND=4)    :: ncrnf, idrnf,idcoef

  CHARACTER(LEN=255) :: cf_clv='GrIS_clv.nc'
  INTEGER(KIND=4)    :: ncclv, idclv

  CHARACTER(LEN=255) :: cf_isf='GrIS_ISF.nc'
  INTEGER(KIND=4)    :: ncisf, idisf,iddepmin,iddepmax

  LOGICAL            :: lnc4 = .FALSE.

  !--------------------------------------------------------------------
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' usage : GrIS_CreateNemoBis.x -g GrIS-file -m MESH_MASK_file [-or RUNOFF-file]'
     PRINT *,'                     [-oc CALVING-file] [-oi ISF-file]'
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       Create NEMO input files related to runoff, calving and ISF '
     PRINT *,'       parameterization. Also create a rnf_dep file.'
     PRINT *,'      '
     PRINT *,'       Reading the GrIS file for liquid discharge, and depth, we put all'
     PRINT *,'       liquid discharge with 0 depth into the runoff file. All other '
     PRINT *,'       liquid discharge with non zero depth are put into an ISF like'
     PRINT *,'       input file. Calving is put into a calving file.'
     PRINT *,'      '
     PRINT *,'       In this program, when a single NEMO point receives multiple in-depth'
     PRINT *,'       contributions (liquid or solid), the contributions are summed up and'
     PRINT *,'       the maximum depht of the contributing discharges is kept in the dep_rnf'
     PRINT *,'       file.'
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'        -g GrIS-file : pass the name of the input GrIS file, following the'
     PRINT *,'                 file format (and units) proposed by Jeremie Mouginot et al.' 
     PRINT *,'        -m MESH_MASK-file : pass the name of the configuration mesh_mask file.'
     PRINT *,'                 It will be used for setting up the size of the output file,'
     PRINT *,'                 setting up nav_lon,nav_lat variables, and for checking the'
     PRINT *,'                 masked points.'
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'         -or RUNOFF-file : name of the output runoff file instead of the'
     PRINT *,'             default ',TRIM(cf_rnf)
     PRINT *,'         -oc CALVING-file : name of the output calving file instead of the'
     PRINT *,'             default ',TRIM(cf_clv)
     PRINT *,'         -oi ISF-file : name of the output calving file instead of the'
     PRINT *,'             default ',TRIM(cf_isf)
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'        Those passed in the arguments.' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf files : ', TRIM(cf_rnf),' ',TRIM(cf_clv),' ',TRIM(cf_isf)
     PRINT *,'               unless some flavour of the -ox options is used.'
     PRINT *,'      '
     PRINT *,'     SEE ALSO :'
     PRINT *,'      All GrIS_xxx programms !' 
     PRINT *,'      '
     STOP
  ENDIF

  ijarg = 1 
  DO WHILE ( ijarg <= narg )
     CALL getarg(ijarg, cldum ) ; ijarg=ijarg+1
     SELECT CASE ( cldum )
     CASE ( '-g'   ) ; CALL getarg(ijarg, cf_GrIS ) ; ijarg=ijarg+1
     CASE ( '-m'   ) ; CALL getarg(ijarg, cf_msh  ) ; ijarg=ijarg+1
        ! option
     CASE ( '-or'  ) ; CALL getarg(ijarg, cf_rnf  ) ; ijarg=ijarg+1
     CASE ( '-oc'  ) ; CALL getarg(ijarg, cf_clv  ) ; ijarg=ijarg+1
     CASE ( '-oi'  ) ; CALL getarg(ijarg, cf_isf  ) ; ijarg=ijarg+1
     CASE ( '-nc4' ) ; lnc4 = .TRUE.
     CASE DEFAULT    ; PRINT *, ' ERROR : ', TRIM(cldum),' : unknown option.'; STOP 1
     END SELECT
  ENDDO

  ! Get GrIS dimensions, allocate and read related variables
  CALL GetGrISInfo(cf_GrIS)  ! variables, npoints, nptim,  nii, nij, dlon, dlat, nitime, drnfdep, drnf, dcalv

  ! Get Model Information : dimensions, allocate and read related variables
  CALL GetModelInfo(cf_msh)  ! variables npiglo, npjglo, glamt, gphit, 

  ! Prepare output files : 
  CALL CreateOutput(cf_rnf)   ! variables : nav_lon, nav_lat, sorunoff, socoefr
  CALL CreateOutput(cf_clv)   ! variables : nav_lon, nav_lat, sorunoff, socoefr
  CALL CreateOutput(cf_isf)   ! variables : nav_lon, nav_lat, sorunoff, socoefr

  !  REAL(KIND=4),    DIMENSION(:,:), ALLOCATABLE :: rnf, rnfmask, rnfisf, hmin, hmax, rcalv
  ALLOCATE (  rnf(npiglo, npjglo), rnfmask(npiglo,npjglo) )
  ALLOCATE (  rnfisf(npiglo, npjglo), hmin(npiglo,npjglo), hmax(npiglo,npjglo) )
  ALLOCATE (  rcalv(npiglo, npjglo) )

  ! initialization to 0 all
  rnf = 0. ; rnfmask = 0. ; rnfisf = 0. ; hmin = 0. ; hmax = 0. ; rcalv = 0. 
  ! conversion factor from GT/month to kg/s : 1GT = 10^12 kg, 1 month = 30 days = 30 * 86400 seconds
  dconv = 1.d12/30.d0/86400.d0

  DO jt = 1, nptim
     ! initialization to 0 all
     rnf = 0. ; rnfisf = 0. ; rcalv = 0. 
     DO ji = 1, npoints 
        ii = nii(ji)
        ij = nij(ji)
        rnfmask(ii,ij) = 0.5
        darea = e1t(ii,ij)* e2t(ii,ij)
        IF (  drnfdep (ji ) == 0 ) THEN
           rnf(ii,ij) = rnf(ii,ij) + drnf(jt,ji) * dconv / darea
        ELSE
           ! BIS version : rnfisf in only 1/2 of the calving
           rnfisf(ii,ij) = rnfisf(ii,ij) + 0.5 * dcalv(jt,ji) * dconv /darea 
           ! BIS version : rnf also take the part of drnf not due to calving.
           rnf(ii,jj) = rnf(ii,jj) + ( drnf(jt,ji) - 0.5 * dcalv(jt,ji) ) *dconv /darea
           hmax(ii,ij) = MAX( hmax(ii,ij), drnfdep(ji) )
        ENDIF
        rcalv(ii,ij) = rcalv(ii,ij) + dcalv(jt,ji) * 12.d0   ! GT/year
     ENDDO
     ierr = NF90_PUT_VAR(ncrnf, idrnf,  rnf  , start=(/1,1,jt/), count=(/npiglo,npjglo,1/)  )
     ierr = NF90_PUT_VAR(ncisf, idisf, rnfisf, start=(/1,1,jt/), count=(/npiglo,npjglo,1/)  )
     ierr = NF90_PUT_VAR(ncclv, idclv, rcalv,  start=(/1,1,jt/), count=(/npiglo,npjglo,1/)  )

  ENDDO
  ! output to file
  ierr = NF90_PUT_VAR(ncrnf, idcoef, rnfmask )
  ierr = NF90_CLOSE(ncrnf)

  ierr = NF90_PUT_VAR(ncisf, iddepmin, hmin  )
  ierr = NF90_PUT_VAR(ncisf, iddepmax, hmax  )
  ierr = NF90_CLOSE(ncisf)

  ierr = NF90_CLOSE(ncclv)


CONTAINS

  SUBROUTINE GetGrISInfo(cd_file)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE  GetGrISInfo ***
    !!
    !! ** Purpose : Extract all relevant information from the GrIS file passed
    !!          as argument.  
    !!
    !! ** Method  :  Read the dimension, allocate GrIS variables, read the variables
    !!          and close the file. All variables are global. 
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_file

    INTEGER(KIND=4) :: ncid, id, ierr
    !!----------------------------------------------------------------------

    ! OPEN file
    ierr = NF90_OPEN(cd_file,NF90_NOWRITE,ncid)
    ! read dimensions (name, time)
    ierr = NF90_INQ_DIMID(ncid,'name',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=npoints )
    ierr = NF90_INQ_DIMID(ncid,'time',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=nptim   )
    PRINT *, 'File : ', TRIM(cd_file)
    PRINT *,'   NPOINTS = ', npoints
    PRINT *,'   NPTIM   = ', nptim
    ! Allocate arrays
    ALLOCATE ( nii(npoints), nij(npoints), ndouble(npoints), ldouble(npoints), nitime( nptim)     )
    ALLOCATE ( dlon(npoints), dlat(npoints), drnfdep(npoints) )
    ALLOCATE ( drnf(nptim,npoints), dcalv(nptim,npoints) )
    !
    ! read data
    ierr = NF90_INQ_VARID(ncid,'x_pix_nemo'  , id) ; ierr = NF90_GET_VAR(ncid, id, nii     )
    ierr = NF90_INQ_VARID(ncid,'y_pix_nemo'  , id) ; ierr = NF90_GET_VAR(ncid, id, nij     )
    ierr = NF90_INQ_VARID(ncid,'lon_nemo'    , id) ; ierr = NF90_GET_VAR(ncid, id, dlon    )
    ierr = NF90_INQ_VARID(ncid,'lat_nemo'    , id) ; ierr = NF90_GET_VAR(ncid, id, dlat    )
    ierr = NF90_INQ_VARID(ncid,'rnfdep'      , id) ; ierr = NF90_GET_VAR(ncid, id, drnfdep )
    !  NetCDF: Attempt to convert between text & numbers 
    ierr = NF90_INQ_VARID(ncid,'time'        , id) ; ierr = NF90_GET_VAR(ncid, id, nitime  )

    ierr = NF90_INQ_VARID(ncid,'rnf_nemo'    , id) ; ierr = NF90_GET_VAR(ncid, id, drnf    )
    ierr = NF90_INQ_VARID(ncid,'iceberg_nemo', id) ; ierr = NF90_GET_VAR(ncid, id, dcalv   )

    ierr = NF90_CLOSE(ncid)

    ! add 1 to nii and nij for fortran use (NEMO index)
    nii(:) = nii(:) + 1
    nij(:) = nij(:) + 1

  END SUBROUTINE GetGrISInfo

  SUBROUTINE GetModelInfo(cd_file)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetModelInfo  ***
    !!
    !! ** Purpose :  Extract all relevant model information from the meshmask
    !!       file passed as argument.
    !!
    !! ** Method  :  open the file, get dimension, allocate variables, 
    !!       read relevant variables, close the file.
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_file

    INTEGER(KIND=4) :: ncid, id, ierr
    !!----------------------------------------------------------------------
    ierr = NF90_OPEN(cd_file, NF90_NOWRITE,ncid)
    ierr = NF90_INQ_DIMID(ncid,'x',id)  ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npiglo)
    ierr = NF90_INQ_DIMID(ncid,'y',id)  ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npjglo)

    ALLOCATE( glamt( npiglo,npjglo), gphit(npiglo, npjglo) )
    ALLOCATE(   e1t( npiglo,npjglo),   e2t(npiglo, npjglo) )

    ierr = NF90_INQ_VARID(ncid,'glamt',id) ; ierr = NF90_GET_VAR(ncid,id, glamt )
    ierr = NF90_INQ_VARID(ncid,'gphit',id) ; ierr = NF90_GET_VAR(ncid,id, gphit )
    ierr = NF90_INQ_VARID(ncid,'e1t'  ,id) ; ierr = NF90_GET_VAR(ncid,id, e1t   )
    ierr = NF90_INQ_VARID(ncid,'e2t'  ,id) ; ierr = NF90_GET_VAR(ncid,id, e2t   )

    ierr = NF90_CLOSE(ncid)

  END SUBROUTINE GetModelInfo

  SUBROUTINE CreateOutput(cd_file)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CreateOutput  ***
    !!
    !! ** Purpose :  Create outputfile whose name is passed as argument 
    !!
    !! ** Method  :  Create different variables according to the name of the file. 
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_file

    INTEGER(KIND=4) :: ierr, ncid, id, idx, idy, idt
    INTEGER(KIND=4) :: idlon, idlat, idtim
    !!----------------------------------------------------------------------
    ierr = NF90_CREATE(cd_file, NF90_NETCDF4, ncid)

    ierr = NF90_DEF_DIM(ncid,'x', npiglo, idx)
    ierr = NF90_DEF_DIM(ncid,'y', npjglo, idy)
    ierr = NF90_DEF_DIM(ncid,'time_counter', NF90_UNLIMITED, idt)

    IF ( lnc4 ) THEN
       ierr = NF90_DEF_VAR(ncid,'nav_lon',     NF90_FLOAT,  (/idx,idy/), idlon, deflate_level=1, chunksizes=(/npiglo,npjglo/) )
       ierr = NF90_DEF_VAR(ncid,'nav_lat',     NF90_FLOAT,  (/idx,idy/), idlat, deflate_level=1, chunksizes=(/npiglo,npjglo/) )
       ierr = NF90_DEF_VAR(ncid,'time_counter',NF90_DOUBLE, (/idt/)    , idtim, deflate_level=1, chunksizes=(/1/)             )
    ELSE
       ierr = NF90_DEF_VAR(ncid,'nav_lon',     NF90_FLOAT,  (/idx,idy/), idlon )
       ierr = NF90_DEF_VAR(ncid,'nav_lat',     NF90_FLOAT,  (/idx,idy/), idlat )
       ierr = NF90_DEF_VAR(ncid,'time_counter',NF90_DOUBLE, (/idt/)    , idtim )
    ENDIF
    ! Attributes
    ierr = NF90_PUT_ATT(ncid, idlon,'long_name' ,'Longitude'  )
    ierr = NF90_PUT_ATT(ncid, idlon,'short_name','nav_lon'    )
    ierr = NF90_PUT_ATT(ncid, idlon,'units'     ,'degree East')

    ierr = NF90_PUT_ATT(ncid, idlat,'long_name' ,'Latitude'   )
    ierr = NF90_PUT_ATT(ncid, idlat,'short_name','nav_lat'    )
    ierr = NF90_PUT_ATT(ncid, idlat,'units'     ,'degree West')

    ierr = NF90_PUT_ATT(ncid, idtim,'units','days since 1950-01-01 00:00:00')
    ierr = NF90_PUT_ATT(ncid, idtim,'calendar','proleptic_gregorian')

    IF      ( cd_file == cf_rnf ) THEN
       ncrnf = ncid
       IF ( lnc4) THEN
          ierr = NF90_DEF_VAR(ncid,'sorunoff',NF90_FLOAT, (/idx,idy,idt/), idrnf,  deflate_level=1, chunksizes=(/npiglo,npjglo,1/) )
          ierr = NF90_DEF_VAR(ncid,'socoefr', NF90_FLOAT, (/idx,idy/)    , idcoef, deflate_level=1, chunksizes=(/npiglo,npjglo/)   )
       ELSE
          ierr = NF90_DEF_VAR(ncid,'sorunoff',NF90_FLOAT, (/idx,idy,idt/), idrnf  )
          ierr = NF90_DEF_VAR(ncid,'socoefr', NF90_FLOAT, (/idx,idy/)    , idcoef )
       ENDIF
       ierr = NF90_PUT_ATT(ncid, idrnf,'long_name' ,'Surface runoff' )
       ierr = NF90_PUT_ATT(ncid, idrnf,'short_name','sorunoff'       )
       ierr = NF90_PUT_ATT(ncid, idrnf,'units'     ,'kg/m2/s'        )

       ierr = NF90_PUT_ATT(ncid, idcoef,'long_name' ,'Runoff mask'   )
       ierr = NF90_PUT_ATT(ncid, idcoef,'short_name','socoefr'       )
       ierr = NF90_PUT_ATT(ncid, idcoef,'units'     ,'[0-0.5]'       )

    ELSE IF (cd_file == cf_clv ) THEN
       ncclv = ncid
       IF ( lnc4) THEN
          ierr = NF90_DEF_VAR(ncid,'soicbclv',NF90_FLOAT, (/idx,idy,idt/), idclv,  deflate_level=1, chunksizes=(/npiglo,npjglo,1/) )
       ELSE
          ierr = NF90_DEF_VAR(ncid,'soicbclv',NF90_FLOAT, (/idx,idy,idt/), idclv  )
       ENDIF
       ierr = NF90_PUT_ATT(ncid, idclv,'long_name' ,'Iceberg Calving Rate' )
       ierr = NF90_PUT_ATT(ncid, idclv,'short_name','soicbclv'             )
       ierr = NF90_PUT_ATT(ncid, idclv,'units'     ,'GT/year'              )
    ELSE IF (cd_file == cf_isf ) THEN
       ncisf = ncid
       IF ( lnc4) THEN
          ierr = NF90_DEF_VAR(ncid,'sornfisf', NF90_FLOAT, (/idx,idy,idt/), idisf,    deflate_level=1, chunksizes=(/npiglo,npjglo,1/) )
          ierr = NF90_DEF_VAR(ncid,'sozisfmin',NF90_FLOAT, (/idx,idy/)    , iddepmin, deflate_level=1, chunksizes=(/npiglo,npjglo/)   )
          ierr = NF90_DEF_VAR(ncid,'sozisfmax',NF90_FLOAT, (/idx,idy/)    , iddepmax, deflate_level=1, chunksizes=(/npiglo,npjglo/)   )
       ELSE
          ierr = NF90_DEF_VAR(ncid,'sornfisf', NF90_FLOAT, (/idx,idy,idt/), idisf    )
          ierr = NF90_DEF_VAR(ncid,'sozisfmin',NF90_FLOAT, (/idx,idy/)    , iddepmin )
          ierr = NF90_DEF_VAR(ncid,'sozisfmax',NF90_FLOAT, (/idx,idy/)    , iddepmax )
       ENDIF
       ierr = NF90_PUT_ATT(ncid, idisf,'long_name' ,'Ice Shelf Melting rate' )
       ierr = NF90_PUT_ATT(ncid, idisf,'short_name','sornfisf'               )
       ierr = NF90_PUT_ATT(ncid, idisf,'units'     ,'kg/m2/s'                )

       ierr = NF90_PUT_ATT(ncid, iddepmin,'long_name' ,'Minimum Runoff depth')
       ierr = NF90_PUT_ATT(ncid, iddepmin,'short_name','sozisfmin'           )
       ierr = NF90_PUT_ATT(ncid, iddepmin,'units'     ,'m'                   )

       ierr = NF90_PUT_ATT(ncid, iddepmax,'long_name' ,'Maximum Runoff depth')
       ierr = NF90_PUT_ATT(ncid, iddepmax,'short_name','sozisfmax'           )
       ierr = NF90_PUT_ATT(ncid, iddepmax,'units'     ,'m'                   )
    ELSE
       PRINT *,' E R R O R: cannot create ',TRIM(cd_file) ; STOP 99
    ENDIF
    ! attributes 
    ierr = NF90_ENDDEF(ncid)

    ierr = NF90_PUT_VAR(ncid, idlon, glamt )
    ierr = NF90_PUT_VAR(ncid, idlat, gphit )
    ierr = NF90_PUT_VAR(ncid, idtim, nitime)

  END SUBROUTINE CreateOutput

END PROGRAM GrIS_CreateNemoBis
