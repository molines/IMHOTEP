PROGRAM mkbathy05
  !!======================================================================
  !!                     ***  PROGRAM  mkbathy05  ***
  !!=====================================================================
  !!  ** Purpose :  Interpolate eORCA025 on eORCA05 grid
  !!                for the northern part use native ORCA05 bathymetry
  !!
  !!  ** Method  :  9 points interpolation, using weight proportional to area
  !!
  !! History :  1.0  : 03/2021  : J.M. Molines : 
  !!----------------------------------------------------------------------
  USE netcdf
  !!----------------------------------------------------------------------
  !! CDFTOOLS_4.0 , MEOM 2017
  !! $Id$
  !! Copyright (c) 2012, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/CDFTOOLSCeCILL.txt)
  !!----------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER(KIND=4)   :: ji,jj
  INTEGER(KIND=4)   :: ii25, ij25
  INTEGER(KIND=4)   :: npi25, npj25
  INTEGER(KIND=4)   :: npi05, npj05
  INTEGER(KIND=4)   :: npi5, npj5
  INTEGER(KIND=4)   :: ierr, ncid, id
  INTEGER(KIND=4)   :: ncidc, idbat, idisf, idraf

  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: v2d25
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: v2d5, rlon5, rlat5
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: bathy05

  CHARACTER(LEN=80) :: cf_bathy25 = 'eORCA025_bathymetry_b0.2_closed_seas.nc'
  CHARACTER(LEN=80) :: cf_bathy5  = 'eORCA05_bathymetry_b0.2_closed_seas.nc'
  CHARACTER(LEN=80) :: cf_bathy05 = 'ORCA05_bathy_meter_v2.nc'
  CHARACTER(LEN=80) :: cf_coo5    = 'eORCA05_coordinates.nc'

  !!--------------------------------------------------------------------
  ! Read Bathy of ORCA05 
  ierr = NF90_OPEN(cf_bathy05,NF90_NOWRITE,ncid)
  ierr = NF90_INQ_DIMID(ncid,'x',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id,len=npi05)
  ierr = NF90_INQ_DIMID(ncid,'y',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id,len=npj05)
  PRINT *, "NPI05 = ", npi05
  PRINT *, "NPJ05 = ", npj05
  ALLOCATE( bathy05(npi05,npj05) )
  ierr = NF90_INQ_VARID(ncid,'Bathymetry',id) ; ierr = NF90_GET_VAR(ncid,id,bathy05,start=(/1,1/), count=(/npi05,npj05/) )
  ierr = NF90_CLOSE(ncid)

  ! Open eORCA025 bathy file
  ierr = NF90_OPEN(cf_bathy25,NF90_NOWRITE,ncid)
  ierr = NF90_INQ_DIMID(ncid,'x',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id,len=npi25)
  ierr = NF90_INQ_DIMID(ncid,'y',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id,len=npj25)
  PRINT *, "NPI25 = ", npi25
  PRINT *, "NPJ25 = ", npj25

  ! Open eORCA05 coordinate file for dimension and lon lat
  ierr = NF90_OPEN(cf_coo5,NF90_NOWRITE,ncidc)
  ierr = NF90_INQ_DIMID(ncidc,'x',id) ; ierr = NF90_INQUIRE_DIMENSION(ncidc,id,len=npi5)
  ierr = NF90_INQ_DIMID(ncidc,'y',id) ; ierr = NF90_INQUIRE_DIMENSION(ncidc,id,len=npj5)
  PRINT *, "NPI5 = ", npi5
  PRINT *, "NPJ5 = ", npj5

  ALLOCATE (v2d25(npi25,npj25)                                 )
  ALLOCATE (v2d5(npi5,npj5), rlon5(npi5,npj5), rlat5(npi5,npj5))
  ! Read lon lat for eORCA05
  ierr = NF90_INQ_VARID(ncidc,'glamt',id)  ; ierr = NF90_GET_VAR(ncidc, id, rlon5, start=(/1,1,1/), count=(/npi5,npj5,1/) )
  ierr = NF90_INQ_VARID(ncidc,'gphit',id)  ; ierr = NF90_GET_VAR(ncidc, id, rlat5, start=(/1,1,1/), count=(/npi5,npj5,1/) )
  ierr = NF90_CLOSE(ncidc)

  CALL Create5 (cf_bathy5)

! float Bathymetry(y, x) ;
  ierr = NF90_INQ_VARID(ncid,'Bathymetry',id )     ;  ierr = NF90_GET_VAR(ncid, id, v2d25 , start=(/1,1/), count=(/npi25,npj25/) )
  CALL Int25to5 (v2d25,v2d5)
  CALL Merge5(v2d5)
  ierr = NF90_PUT_VAR( ncidc, idbat, v2d5, start=(/1,1/), count=(/npi5,npj5/) )
! float Bathymetry_isf(y, x) ;
  ierr = NF90_INQ_VARID(ncid,'Bathymetry_isf',id ) ;  ierr = NF90_GET_VAR(ncid, id, v2d25 , start=(/1,1/), count=(/npi25,npj25/) )
  CALL Int25to5 (v2d25,v2d5)
  CALL Merge5(v2d5)
  ierr = NF90_PUT_VAR( ncidc, idisf, v2d5, start=(/1,1/), count=(/npi5,npj5/) )
! float isf_draft(y, x) ;
  ierr = NF90_INQ_VARID(ncid,'isf_draft',id )      ;  ierr = NF90_GET_VAR(ncid, id, v2d25 , start=(/1,1/), count=(/npi25,npj25/) )
  CALL Int25to5 (v2d25,v2d5)
  ierr = NF90_PUT_VAR( ncidc, idraf, v2d5, start=(/1,1/), count=(/npi5,npj5/) )

  ierr = NF90_CLOSE(ncid) 
  ierr = NF90_CLOSE(ncidc) 

  CONTAINS

  SUBROUTINE Create5( cd_file)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Create5  ***
    !!
    !! ** Purpose :  Create output bathy5 file 
    !!
    !! ** Method  :  Netcdf primitives
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_file
    INTEGER(KIND=4) :: idx, idy, idlon, idlat
    ierr = NF90_CREATE(cd_file, NF90_NETCDF4, ncidc)
    ! dimensions
    ierr = NF90_DEF_DIM(ncidc,'x',npi5, idx)
    ierr = NF90_DEF_DIM(ncidc,'y',npj5, idy)
    ! variables lon lat
    ierr = NF90_DEF_VAR(ncidc,'nav_lon', NF90_FLOAT, (/idx,idy/), idlon )
    ierr = NF90_DEF_VAR(ncidc,'nav_lat', NF90_FLOAT, (/idx,idy/), idlat )
    ! data variables
    ierr = NF90_DEF_VAR(ncidc,'Bathymetry'    , NF90_FLOAT, (/idx,idy/), idbat )
    ierr = NF90_DEF_VAR(ncidc,'Bathymetry_isf', NF90_FLOAT, (/idx,idy/), idisf )
    ierr = NF90_DEF_VAR(ncidc,'isf_draft'     , NF90_FLOAT, (/idx,idy/), idraf )
    ! Attributes
    ierr = NF90_PUT_ATT(ncidc,idbat,'_FillValue', -99.)
    ierr = NF90_PUT_ATT(ncidc,idbat,'long_name', 'Median depth by area')
    ierr = NF90_PUT_ATT(ncidc,idbat,'units', 'meters')
    ierr = NF90_PUT_ATT(ncidc,idbat,'valid_max', 10000.)
    ierr = NF90_PUT_ATT(ncidc,idbat,'valid_min', 0.    )

    ierr = NF90_PUT_ATT(ncidc,idisf,'_FillValue', -99.)
    ierr = NF90_PUT_ATT(ncidc,idisf,'long_name', 'Median depth by area')
    ierr = NF90_PUT_ATT(ncidc,idisf,'units', 'meters')
    ierr = NF90_PUT_ATT(ncidc,idisf,'valid_max', 10000.)
    ierr = NF90_PUT_ATT(ncidc,idisf,'valid_min', 0.    )

    ierr = NF90_PUT_ATT(ncidc,idraf,'_FillValue', -99.)
    ierr = NF90_PUT_ATT(ncidc,idraf,'long_name', 'Median depth by area')
    ierr = NF90_PUT_ATT(ncidc,idraf,'units', 'meters')
    ierr = NF90_PUT_ATT(ncidc,idraf,'valid_max', 5000.)
    ierr = NF90_PUT_ATT(ncidc,idraf,'valid_min', 0.    )

    ierr = NF90_PUT_ATT(ncidc,NF90_GLOBAL,'Comment','Created by mkbathy05 &
           & with '//TRIM(cf_bathy25)//' and '//TRIM(cf_bathy05) )
    
    ierr = NF90_ENDDEF(ncidc)

    ! fill lon lat
    ierr = NF90_PUT_VAR(ncidc,idlon,rlon5,start=(/1,1/), count=(/npi5,npj5/) )
    ierr = NF90_PUT_VAR(ncidc,idlat,rlat5,start=(/1,1/), count=(/npi5,npj5/) )

  END SUBROUTINE Create5

  SUBROUTINE Int25to5 (p2d25,p2d5)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Int25to5  ***
    !!
    !! ** Purpose :  interpolate 025 array to 05 array 
    !!
    !! ** Method  :  take care of the weight 
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(in   ) :: p2d25
    REAL(KIND=4), DIMENSION(:,:), INTENT(  out) :: p2d5

    REAL(KIND=4) :: zwgh
    !!----------------------------------------------------------------------
    p2d5 = 0.
    DO jj = 2, npj5 -1
      ij25=2*jj -1
      DO ji = 2, npi5 -1
          ii25=2*ji -1
          p2d5(ji,jj) = ( p2d25(ii25,ij25) + &
             &     0.5* ( p2d25(ii25-1,ij25  ) + p2d25(ii25+1,ij25  ) + p2d25(ii25  ,ij25-1) + p2d25(ii25  ,ij25+1) ) + &
             &    0.25* ( p2d25(ii25-1,ij25-1) + p2d25(ii25+1,ij25-1) + p2d25(ii25+1,ij25+1) + p2d25(ii25-1,ij25+1) ) )
          ! check 
          zwgh =        ( ( p2d25(ii25  ,ij25  ) /= 0. ) + &
             &     0.5* ( ( p2d25(ii25-1,ij25  ) /= 0. ) + ( p2d25(ii25+1,ij25  ) /=0. ) + ( p2d25(ii25  ,ij25-1) /= 0. ) +( p2d25(ii25  ,ij25+1) /= 0. ) ) + &
             &    0.25* ( ( p2d25(ii25-1,ij25-1) /= 0. ) + ( p2d25(ii25+1,ij25-1) /=0. ) + ( p2d25(ii25+1,ij25+1) /= 0. ) +( p2d25(ii25-1,ij25+1) /= 0. ) ) )
          zwgh = - zwgh
          IF ( zwgh /= 0. ) THEN
            p2d5(ji,jj) = p2d5(ji,jj)/zwgh
          ELSE
            p2d5(ji,jj) = 0.
          ENDIF

      ENDDO
    ENDDO
    ! Periodicity
    p2d5(1   ,:) = p2d5(npi5-1,:)
    p2d5(npi5,:) = p2d5(2     ,:)
      
  END SUBROUTINE Int25to5

  SUBROUTINE Merge5 (p2d5)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Merge05  ***
    !!
    !! ** Purpose :   patch northern part of the domain from ORCA05
    !!                to freshly build eORCA05
    !!
    !! ** Method  :   patch north of a common line (hand defined)
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=4), DIMENSION(:,:), INTENT(inout) :: p2d5

    INTEGER(KIND=4) :: ii, ij
    INTEGER(KIND=4) :: ijcommon1=79   ! ORCA05
    INTEGER(KIND=4) :: ijcommon2=172  ! eORCA05
    !!----------------------------------------------------------------------
    ij=ijcommon2
    DO jj=ijcommon1,npj05
       p2d5(:,ij) = bathy05(:,jj)
       ij=ij+1
    ENDDO
    PRINT *, 'IJ final', ij
    

  END SUBROUTINE Merge5

END PROGRAM mkbathy05
