PROGRAM GrISchk
  !!======================================================================
  !!                     ***  PROGRAM  GrISchk  ***
  !!=====================================================================
  !!  ** Purpose : Reader for GrIS file provided by J. Mouginot
  !!               Various checking will be coded from this base.
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
 INTEGER(KIND=4) :: npoints, nptim, npiglo, npjglo, npk
 INTEGER(KIND=4) :: iibase, ijbase
 INTEGER(KIND=4) :: ii, ij, ik, ipb
 INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: nii, nij, ndouble
 INTEGER(KIND=8), DIMENSION(:), ALLOCATABLE :: nitime
 INTEGER(KIND=8), DIMENSION(:,:), ALLOCATABLE :: mbathy
 REAL(KIND=8),    DIMENSION(:), ALLOCATABLE :: dlon, dlat, drnfdep
 REAL(KIND=8),    DIMENSION(:,:), ALLOCATABLE :: drnf, dcalv, dlamt, dphit
 REAL(KIND=4),    DIMENSION(:,:), ALLOCATABLE :: gdepw_0, rdep

 INTEGER(KIND=4) :: ji, jj,jt, jj1
 INTEGER(KIND=4) :: ncid, id, ierr
 CHARACTER(LEN=80) :: cf_in='eORCA025_GrIS_forcing_50percent_solid.nc'
 CHARACTER(LEN=80) :: cf_msh='eORCA025.L75_mesh_mask.nc'
 CHARACTER(LEN=3)  :: cpb
 LOGICAL, DIMENSION(:), ALLOCATABLE :: ldouble
 !--------------------------------------------------------------------
 ! OPEN file
 ierr = NF90_OPEN(cf_in,NF90_NOWRITE,ncid)
 ! read dimensions (name, time)
 ierr = NF90_INQ_DIMID(ncid,'name',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=npoints )
 ierr = NF90_INQ_DIMID(ncid,'time',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=nptim   )
 PRINT *, 'File : ', TRIM(cf_in)
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
 ! add 1 to nii and nij for fortran use
 nii(:) = nii(:) + 1
 nij(:) = nij(:) + 1

 ndouble = 0
 ldouble = .false.
 DO ji = 1, npoints
   iibase=nii(ji)
   ijbase=nij(ji)
   DO jj=ji+1, npoints
     IF ( .not. ldouble(jj) ) THEN
     IF (nii(jj) == iibase ) THEN
       IF (nij(jj) == ijbase ) THEN
          PRINT '(a,i4,a,i4,2i5,2f9.2)',' point ', jj, ' identical to ',ji ,iibase, ijbase,drnfdep(ji), drnfdep(jj)
          ndouble(ji) = ndouble(ji) + 1
          ldouble(jj)=.true.
       ENDIF
     ENDIF
     ENDIF
    ENDDO
 ENDDO

DO ji =1, npoints
!   IF ( ndouble(ji) /= 0 ) THEN
     PRINT *, ' point ',ji, 'repeated ',ndouble(ji) ,' times'
!  ENDIF
ENDDO

END PROGRAM GrISchk
