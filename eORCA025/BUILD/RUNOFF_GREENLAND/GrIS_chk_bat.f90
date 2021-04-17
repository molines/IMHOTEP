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
 INTEGER(KIND=4) :: ii, ij, ik, ipb
 INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: nii, nij
 INTEGER(KIND=8), DIMENSION(:), ALLOCATABLE :: nitime
 INTEGER(KIND=8), DIMENSION(:,:), ALLOCATABLE :: mbathy
 REAL(KIND=8),    DIMENSION(:), ALLOCATABLE :: dlon, dlat, drnfdep
 REAL(KIND=8),    DIMENSION(:,:), ALLOCATABLE :: drnf, dcalv, dlamt, dphit
 REAL(KIND=4),    DIMENSION(:,:), ALLOCATABLE :: gdepw_0, rdep

 INTEGER(KIND=4) :: ji, jt, jj1
 INTEGER(KIND=4) :: ncid, id, ierr
 CHARACTER(LEN=80) :: cf_in='eORCA025_GrIS_forcing_50percent_solid.nc'
 CHARACTER(LEN=80) :: cf_msh='eORCA025_bathymetry_b0.2_closed_seas_greenland.nc'
 CHARACTER(LEN=3)  :: cpb
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
 ALLOCATE ( nii(npoints), nij(npoints), nitime( nptim)     )
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

 ierr = NF90_OPEN(cf_msh,NF90_NOWRITE,ncid)
 ierr = NF90_INQ_DIMID(ncid,'x',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=npiglo )
 ierr = NF90_INQ_DIMID(ncid,'y',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=npjglo )

 ALLOCATE(dlamt(npiglo,npjglo), dphit(npiglo,npjglo), gdepw_0(npiglo, npjglo), mbathy(npiglo, npjglo) )
 ALLOCATE(rdep(npiglo,npjglo) )
 ierr = NF90_INQ_VARID(ncid,'Bathymetry', id)  ; ierr = NF90_GET_VAR(ncid, id, rdep    )

 DO ji=1, npoints
    ii=nii(ji)  ; ij=nij(ji) 
!    PRINT *, dlon(ji), dlamt(ii,ij), dlon(ji) - dlamt(ii,ij)
 ENDDO 

!DO ji=1,npoints
!   PRINT *, ji, MAXVAL(drnf(:,ji)), MINVAL(drnf(:,ji))
!   PRINT *, ji, MAXVAL(dcalv(:,ji)), MINVAL(dcalv(:,ji))
!ENDDO

ipb=0
 DO ji = 1, npoints
    ii=nii(ji)  ; ij=nij(ji)  
     IF (rdep(ii,ij) < drnfdep(ji) ) THEN
       ipb=ipb+1
       WRITE(cpb,'(i3)') ipb
       PRINT *,'PB'//adjustl(cpb)
! ncks -d x,897,1123 -d y,971,1203
       PRINT '(i4,2f5.0,2I5,2f9.3)', ji, rdep(ii,ij), drnfdep(ji), ii-897,ij-971,dlon(ji),dlat(ji)
         DO jj1 = ij+3, ij-3, -1
           PRINT '(7f5.0)', rdep( ii-3:ii+3, jj1 )
         ENDDO
           
     ELSE
      continue
     ENDIF
 ENDDO


! DO jt=1, nptim
!   PRINT *, jt, nitime(jt)
!ENDDO
 
 

 
END PROGRAM GrISchk
