PROGRAM create_rnf_dep_mask
  !!======================================================================
  !!                     ***  PROGRAM  create_rnf_dep_mask ***
  !!=====================================================================
  !!  ** Purpose :  Create depth and mask file for runoff
  !!
  !!  ** Method  : Read the 3 sources and produce 2 files : rnf_dep.nc and rnf_mask.nc
  !!               Names of files and variables are hard coded because this
  !!               program is likely to be used once ...
  !!               Need to be recompiled if change in names (files and variables).
  !!
  !! History :  1.0  : 04/2021  : J.M. Molines : 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------
  USE netcdf
  !!----------------------------------------------------------------------
  !! $Id$
  !! Copyright (c) 2012, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/CDFTOOLSCeCILL.txt)
  !!----------------------------------------------------------------------
  INTEGER(KIND=4) :: ierr, ncid, id
  INTEGER(KIND=4) :: ncidd, idd1,idd2 , idxd, idyd, idlond, idlatd
  INTEGER(KIND=4) :: ncidm, idm,        idxm, idym
  INTEGER(KIND=4) :: npiglo, npjglo 
  INTEGER(KIND=4) :: narg, ijarg, iargc

  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rlon, rlat
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rdepmin, rdepmax, rmsk
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rdepISBAmin, rdepISBAmax, rmskISBA
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rdepGrISmin, rdepGrISmax, rmskGrIS
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rdepAntamin, rdepAntamax, rmskAnta

  LOGICAL :: ll_use_namelist = .FALSE.

  CHARACTER(LEN=80) :: cldum, cf_namelist='create_rnf_dep_mask.namelist'

  CHARACTER(LEN=80) :: cf_rnfGrIs_dep = 'eORCA025.L75_y1950-2020_1m_greenland_isfbis.nc'
  CHARACTER(LEN=80) ::    cv_rnfGrIs_depmin = 'sozisfmin'
  CHARACTER(LEN=80) ::    cv_rnfGrIs_depmax = 'sozisfmax'
  CHARACTER(LEN=80) :: cf_rnfISBA_dep = 'eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc'
  CHARACTER(LEN=80) ::    cv_rnfISBA_depmin = 'N/A'
  CHARACTER(LEN=80) ::    cv_rnfISBA_depmax = 'N/A'
  CHARACTER(LEN=80) :: cf_rnfAnta_dep = 'eORCA025_rnfisf_b0.2_c3.0_d1.0_v0.0.nc'
  CHARACTER(LEN=80) ::    cv_rnfAnta_depmin = 'sozisfmin'
  CHARACTER(LEN=80) ::    cv_rnfAnta_depmax = 'sozisfmax'

  CHARACTER(LEN=80) :: cf_rnfGrIs_msk = 'eORCA025.L75_y1950-2020_1m_greenland_rnfbis.nc'
  CHARACTER(LEN=80) ::    cv_rnfGrIs_msk = 'socoefr'
  CHARACTER(LEN=80) :: cf_rnfISBA_msk = 'eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc'
  CHARACTER(LEN=80) ::    cv_rnfISBA_msk = 'socoefr'
  CHARACTER(LEN=80) :: cf_rnfAnta_msk = 'eORCA025_mskisf_b0.2_c3.0_d1.0_v0.0.nc'
  CHARACTER(LEN=80) ::    cv_rnfAnta_msk = 'mask_isf_front'


  CHARACTER(LEN=80) :: cf_rnf_dep = 'eORCA025.L75_rnf_dep.nc'
  CHARACTER(LEN=80) ::    cv_rnf_depmin = 'sozisfmin'
  CHARACTER(LEN=80) ::    cv_rnf_depmax = 'sozisfmax'
  CHARACTER(LEN=80) :: cf_rnf_msk = 'eORCA025.L75_rnf_msk.nc'
  CHARACTER(LEN=80) ::    cv_rnf_msk = 'socoefr'

  ! Dimensions name :
  CHARACTER(LEN=80) :: cl_x='x' , cl_y='y'
  CHARACTER(LEN=80) :: cv_lon='nav_lon' , cv_lat='nav_lat'

  NAMELIST /namcrdm/ cf_rnfGrIs_dep, cv_rnfGrIs_depmin, cv_rnfGrIs_depmax, &
                     cf_rnfISBA_dep, cv_rnfISBA_depmin, cv_rnfISBA_depmax, &
                     cf_rnfAnta_dep, cv_rnfAnta_depmin, cv_rnfAnta_depmax, &
                     cf_rnf_dep    , cv_rnf_depmin    , cv_rnf_depmax,     &
                     cf_rnfGrIs_msk, cv_rnfGrIs_msk,                       &
                     cf_rnfISBA_msk, cv_rnfISBA_msk,                       &
                     cf_rnfAnta_msk, cv_rnfAnta_msk,                       &
                     cf_rnf_msk    , cv_rnf_msk,                           &
                     cl_x          , cv_lon,                               &
                     cl_y          , cv_lat

  ! -----------------------------------------------------------------------------------------
  narg = iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' usage : create_rnf_dep_mask.x -f NAMELIST-name '
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       Create rnf_dep and rnf_mask files from various sources:' 
     PRINT *,'          ISBA : for river runoff (except antarctica and Greenland)'
     PRINT *,'          GrIS : for river runoff, glacier and isf melting, calving'
     PRINT *,'          Anta : for iceshelf melting parametrisation (Rignot)'
     PRINT *,'        depth are given in meters, and runoff mask is 0 everuwhere, except'
     PRINT *,'        on grid points where runoff (or isf runoff) is applied. For those'
     PRINT *,'        latter points, runoff mask takes the value 0.5 (for historical reasons)'
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'       -f NAMELIST-name : If this file does not exist, takes the default values'
     PRINT *,'              which corresponds to eORCA025-IMHOTEP configuration (2021)' 
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'       none' 
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'        none'
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'           If not changed in the namelist :'
     PRINT *,'       netcdf file : ', TRIM(cf_rnf_msk),' and ',TRIM(cf_rnf_dep)
     PRINT *,'         variables : ', TRIM(cv_rnf_msk),' and ', TRIM(cv_rnf_depmin),', ',TRIM(cv_rnf_depmax)
     PRINT *,'      '
     PRINT *,'      '
     STOP
  ENDIF

  ijarg = 1 
  DO WHILE ( ijarg <= narg )
     CALL getarg(ijarg, cldum ) ; ijarg=ijarg+1
     SELECT CASE ( cldum )
     CASE ( '-f'   ) ; CALL getarg(ijarg, cf_namelist ) ; ijarg=ijarg+1
     END SELECT
  ENDDO

 INQUIRE(file=cf_namelist, exist= ll_use_namelist ) 
 IF (ll_use_namelist) THEN
   PRINT *,' Reading namelist ', TRIM(cf_namelist)
   OPEN(inum,file=cf_namelist)
   READ(inum,namcrdm)
   CLOSE (inum)
 ENDIF

  ! GrIS msk
  !=====
  ierr = NF90_OPEN(cf_rnfGrIS_msk,NF90_NOWRITE, ncid)
  ierr = NF90_INQ_DIMID(ncid,cl_x,id)  ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npiglo)
  ierr = NF90_INQ_DIMID(ncid,cl_y,id)  ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npjglo)
  PRINT *,' NPIGLO = ', npiglo
  PRINT *,' NPJGLO = ', npjglo
  ! Allocate all array with same size (reasonable assumption ! )
  ALLOCATE ( rlon( npiglo, npjglo)       ,  rlat(npiglo, npjglo) )
  ALLOCATE ( rdepmin( npiglo, npjglo)    ,  rdepmax( npiglo, npjglo)    ,  rmsk( npiglo, npjglo) )
  ALLOCATE ( rdepISBAmin( npiglo, npjglo),  rdepISBAmax( npiglo, npjglo),  rmskISBA( npiglo, npjglo) )
  ALLOCATE ( rdepGrISmin( npiglo, npjglo),  rdepGrISmax( npiglo, npjglo),  rmskGrIS( npiglo, npjglo) )
  ALLOCATE ( rdepAntamin( npiglo, npjglo),  rdepAntamax( npiglo, npjglo),  rmskAnta( npiglo, npjglo) )

  rdepmin(:,:) = -1.0  ; rdepmax(:,:) = -1.0  ; rmsk(:,:) = 0.0  

  ierr = NF90_INQ_VARID( ncid, cv_rnfGrIs_msk, id ) ; ierr = NF90_GET_VAR(ncid, id, rmskGrIS )
  ierr = NF90_INQ_VARID( ncid, cv_lon, id )         ; ierr = NF90_GET_VAR(ncid, id, rlon )
  ierr = NF90_INQ_VARID( ncid, cv_lat, id )         ; ierr = NF90_GET_VAR(ncid, id, rlat )
  ierr = NF90_CLOSE(ncid)
  WHERE ( rmskGrIS /= 0 ) rmsk = 0.5

  ! ISBA msk
  ! ========
  ierr = NF90_OPEN(cf_rnfISBA_msk,NF90_NOWRITE, ncid)
  ierr = NF90_INQ_VARID( ncid, cv_rnfISBA_msk, id ) ; ierr = NF90_GET_VAR(ncid, id, rmskISBA )
  ierr = NF90_CLOSE(ncid)
  WHERE ( rmskISBA /= 0 ) rmsk = 0.5

  ! Anta msk
  ! ========
  ierr = NF90_OPEN(cf_rnfAnta_msk,NF90_NOWRITE, ncid)
  ierr = NF90_INQ_VARID( ncid, cv_rnfAnta_msk, id ) ; ierr = NF90_GET_VAR(ncid, id, rmskAnta )
  WHERE ( rmskAnta /= 0 ) rmskAnta = 0.5
  ierr = NF90_CLOSE(ncid)
  WHERE ( rmskAnta /= 0 ) rmsk = 0.5

  ! GrIS dep
  ! ========
  ierr = NF90_OPEN(cf_rnfGrIS_dep,NF90_NOWRITE, ncid)
  ierr = NF90_INQ_VARID( ncid, cv_rnfGrIS_depmin, id ) ; ierr = NF90_GET_VAR(ncid, id, rdepGrISmin )
  ierr = NF90_INQ_VARID( ncid, cv_rnfGrIS_depmax, id ) ; ierr = NF90_GET_VAR(ncid, id, rdepGrISmax )
  ierr = NF90_CLOSE(ncid)
  WHERE( rdepGrISmin /=0 ) rdepmin = rdepGrISmin
  WHERE( rdepGrISmax /=0 ) rdepmax = rdepGrISmax
  WHERE ( rdepmax == 0 )  rdepmax = 10.

  ! ISBA dep
  ! ========
  WHERE( rmskISBA /=0 ) rdepmin = 0.
  WHERE( rmskISBA /=0 ) rdepmax = 10.

  ! Anta dep
  ! ========
  ierr = NF90_OPEN(cf_rnfAnta_dep,NF90_NOWRITE, ncid)
  ierr = NF90_INQ_VARID( ncid, cv_rnfAnta_depmin, id ) ; ierr = NF90_GET_VAR(ncid, id, rdepAntamin )
  ierr = NF90_INQ_VARID( ncid, cv_rnfAnta_depmax, id ) ; ierr = NF90_GET_VAR(ncid, id, rdepAntamax )
  ierr = NF90_CLOSE(ncid)
  WHERE( rdepAntamin /=0 ) rdepmin = rdepAntamin
  WHERE( rdepAntamax /=0 ) rdepmax = rdepAntamax

  ! Create output file
  ierr = NF90_CREATE(cf_rnf_dep,NF90_NETCDF4,ncidd)

  ierr = NF90_DEF_DIM(ncidd, cl_x, npiglo, idxd)
  ierr = NF90_DEF_DIM(ncidd, cl_y, npjglo, idyd)

  ierr = NF90_DEF_VAR(ncidd, cv_lon,        NF90_FLOAT, (/idxd, idyd/), idlond , deflate_level=1)
  ierr = NF90_DEF_VAR(ncidd, cv_lat,        NF90_FLOAT, (/idxd, idyd/), idlatd , deflate_level=1)
  ierr = NF90_DEF_VAR(ncidd, cv_rnf_depmin, NF90_FLOAT, (/idxd, idyd/), idd1   , deflate_level=1)
  ierr = NF90_DEF_VAR(ncidd, cv_rnf_depmax, NF90_FLOAT, (/idxd, idyd/), idd2   , deflate_level=1)

  ierr = NF90_PUT_ATT( ncidd,idlond,'long_name', 'Longitude degrees East' )
  ierr = NF90_PUT_ATT( ncidd,idlatd,'long_name', 'Latitude degrees North' )

  ierr = NF90_PUT_ATT( ncidd,idd1,'long_name', 'minimum runoff dep' )
  ierr = NF90_PUT_ATT( ncidd,idd1,'units'    , 'meters' )

  ierr = NF90_PUT_ATT( ncidd,idd2,'long_name', 'maximum runoff dep' )
  ierr = NF90_PUT_ATT( ncidd,idd2,'units'    , 'meters' )
  ierr = NF90_ENDDEF(ncidd)

  ierr = NF90_PUT_VAR(ncidd,idlond, rlon)
  ierr = NF90_PUT_VAR(ncidd,idlatd, rlat)
  ierr = NF90_PUT_VAR(ncidd,idd1, rdepmin)
  ierr = NF90_PUT_VAR(ncidd,idd2, rdepmax)
  ierr = NF90_CLOSE(ncidd)

  
  ierr = NF90_CREATE(cf_rnf_msk,NF90_NETCDF4,ncidm)
  ierr = NF90_DEF_DIM(ncidm, cl_x,npiglo, idxm)
  ierr = NF90_DEF_DIM(ncidm, cl_y,npjglo, idym)

  ierr = NF90_DEF_VAR(ncidm, cv_lon,        NF90_FLOAT, (/idxm, idym/), idlonm , deflate_level=1)
  ierr = NF90_DEF_VAR(ncidm, cv_lat,        NF90_FLOAT, (/idxm, idym/), idlatm , deflate_level=1)
  ierr = NF90_DEF_VAR(ncidm, cv_rnf_msk, NF90_FLOAT, (/idxm, idym/), idm       , deflate_level=1)

  ierr = NF90_PUT_ATT( ncidm,idlonm,'long_name', 'Longitude degrees East' )
  ierr = NF90_PUT_ATT( ncidm,idlatm,'long_name', 'Latitude degrees North' )

  ierr = NF90_PUT_ATT( ncidm,idm,'long_name', 'minimum runoff dep' )
  ierr = NF90_PUT_ATT( ncidm,idm,'units'    , 'meters' )

  ierr = NF90_ENDDEF(ncidm)

  ierr = NF90_PUT_VAR(ncidm,idlonm, rlon)
  ierr = NF90_PUT_VAR(ncidm,idlatm, rlat)
  ierr = NF90_PUT_VAR(ncidm,idm, rmsk )
  ierr = NF90_CLOSE(ncidm)

END PROGRAM create_rnf_dep_mask
