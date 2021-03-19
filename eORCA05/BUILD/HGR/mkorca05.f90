PROGRAM mkorca05
  !!======================================================================
  !!                     ***  PROGRAM  mkorca05  ***
  !!=====================================================================
  !!  ** Purpose :  Create a horizontal grid for eORCA05 from eORCA025 grid
  !!
  !!  ** Method  :  just think a little bit ...:)
  !!
  !! History :  1.0  : 03/2021  : J.M. Molines : 
  !!----------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  INTEGER(KIND=4) :: ji,jj
  INTEGER(KIND=4) :: ii, ij
  INTEGER(KIND=4) :: npiglo, npjglo
  INTEGER(KIND=4) :: ncid, id, ierr
  INTEGER(KIND=4) :: ii05, ii025, ni05
  INTEGER(KIND=4) :: ij05, ij025, nj05
  INTEGER(KIND=4) :: iistart025, ijstart025, iiend025, ijend025
  INTEGER(KIND=4) :: nidim5, njdim5
  ! common T point 
  INTEGER(KIND=4) :: iiref025=5 , ijref025=697
  INTEGER(KIND=4) :: iiref05=3 , ijref05=256

  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dphit, dlamt, de1t, de2t
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dphiu, dlamu, de1u, de2u
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dphiv, dlamv, de1v, de2v
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dphif, dlamf, de1f, de2f
  ! eORCA05
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dphit5, dlamt5, de1t5, de2t5
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dphiu5, dlamu5, de1u5, de2u5
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dphiv5, dlamv5, de1v5, de2v5
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dphif5, dlamf5, de1f5, de2f5

  CHARACTER(LEN=80) :: cf_domain025='eORCA025.L75_domain_cfg.nc'
  CHARACTER(LEN=80) :: cf_coord05='eORCA05_coordinates.nc'
  CHARACTER(LEN=80) :: cf_domain05='ORCA05_domain_cfg.nc'
  CHARACTER(LEN=10) :: cdimx='x', cdimy='y'
  !!----------------------------------------------------------------------
  CALL GetGrid(cf_domain025)
  CALL GetSize05
  CALL BuildLonLat
  CALL BuildMetric
  CALL ReleaseGrid   ! deallocate grid arrays used for eORCA025
  CALL GetGrid(cf_domain05)
  CALL Merge05
  CALL ReleaseGrid   ! deallocate grid arrays used for ORCA05

  CALL Wcoord

CONTAINS

  SUBROUTINE GetGrid(cd_domain)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetGrid  ***
    !!
    !! ** Purpose :  Read all the horizontal variables of eORCA025 grid 
    !!               Can probably be done readin less (horizontal metrics not used !)
    !!
    !! ** Method  :  Netcdf Primitives 
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_domain

    ierr = NF90_OPEN(cd_domain,NF90_NOWRITE, ncid)
    ! dimensions
    ierr = NF90_INQ_DIMID(ncid, cdimx,id) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=npiglo)
    ierr = NF90_INQ_DIMID(ncid, cdimy,id) ; ierr = NF90_INQUIRE_DIMENSION(ncid,id, len=npjglo)
    PRINT *, ' NPIGLO = ', npiglo
    PRINT *, ' NPJGLO = ', npjglo
    ! Allocate arrays
    ALLOCATE ( dphit(npiglo,npjglo), dlamt(npiglo,npjglo), de1t(npiglo,npjglo), de2t(npiglo,npjglo) )
    ALLOCATE ( dphiu(npiglo,npjglo), dlamu(npiglo,npjglo), de1u(npiglo,npjglo), de2u(npiglo,npjglo) )
    ALLOCATE ( dphiv(npiglo,npjglo), dlamv(npiglo,npjglo), de1v(npiglo,npjglo), de2v(npiglo,npjglo) )
    ALLOCATE ( dphif(npiglo,npjglo), dlamf(npiglo,npjglo), de1f(npiglo,npjglo), de2f(npiglo,npjglo) )

    ! T point
    ierr = NF90_INQ_VARID(ncid,'gphit',id) ; ierr= NF90_GET_VAR(ncid, id, dphit, start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'glamt',id) ; ierr= NF90_GET_VAR(ncid, id, dlamt, start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'e1t'  ,id) ; ierr= NF90_GET_VAR(ncid, id, de1t , start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'e2t'  ,id) ; ierr= NF90_GET_VAR(ncid, id, de2t , start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ! U point
    ierr = NF90_INQ_VARID(ncid,'gphiu',id) ; ierr= NF90_GET_VAR(ncid, id, dphiu, start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'glamu',id) ; ierr= NF90_GET_VAR(ncid, id, dlamu, start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'e1u'  ,id) ; ierr= NF90_GET_VAR(ncid, id, de1u , start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'e2u'  ,id) ; ierr= NF90_GET_VAR(ncid, id, de2u , start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ! V point
    ierr = NF90_INQ_VARID(ncid,'gphiv',id) ; ierr= NF90_GET_VAR(ncid, id, dphiv, start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'glamv',id) ; ierr= NF90_GET_VAR(ncid, id, dlamv, start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'e1v'  ,id) ; ierr= NF90_GET_VAR(ncid, id, de1v , start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'e2v'  ,id) ; ierr= NF90_GET_VAR(ncid, id, de2v , start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ! F point
    ierr = NF90_INQ_VARID(ncid,'gphif',id) ; ierr= NF90_GET_VAR(ncid, id, dphif, start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'glamf',id) ; ierr= NF90_GET_VAR(ncid, id, dlamf, start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'e1f'  ,id) ; ierr= NF90_GET_VAR(ncid, id, de1f , start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ierr = NF90_INQ_VARID(ncid,'e2f'  ,id) ; ierr= NF90_GET_VAR(ncid, id, de2f , start=(/1,1,1/), count=(/npiglo,npjglo,1/) )
    ! close input file
    ierr = NF90_CLOSE(ncid)
  END SUBROUTINE GetGrid

  SUBROUTINE ReleaseGrid
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ReleaseGrid  ***
    !!
    !! ** Purpose :  Deallocate grid arrays before re-use
    !!
    !! ** Method  :  Deallocate global arrays
    !!
    !!----------------------------------------------------------------------

    DEALLOCATE ( dphit, dlamt, de1t, de2t )
    DEALLOCATE ( dphiu, dlamu, de1u, de2u )
    DEALLOCATE ( dphiv, dlamv, de1v, de2v )
    DEALLOCATE ( dphif, dlamf, de1f, de2f )

  END SUBROUTINE ReleaseGrid


  SUBROUTINE GetSize05
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE GetSize05  ***
    !!
    !! ** Purpose :  Try to figure out the size of the eORCA05 grid
    !!
    !! ** Method  :   Start from a common T points and reach the boundaries
    !!
    !!----------------------------------------------------------------------

    ii025 = iiref025
    ni05=1
    ! toward 1
    DO WHILE (ii025 > 2 )
       ii025 = ii025-2
       ni05  = ni05+1
    ENDDO
    iistart025=ii025
    ! toward end
    ii025=iiref025
    DO WHILE (ii025 < npiglo-2 )
       ii025 = ii025 + 2
       ni05  = ni05 + 1
    ENDDO
    iiend025=ii025

    PRINT *, ' NI05 = ', ni05

    ij025=ijref025
    nj05=1
    ! toward 1
    DO WHILE (ij025 > 2 )
       ij025 = ij025-2
       nj05  = nj05+1
    ENDDO
    ijstart025=ij025
    ! toward end
    ij025 = ijref025
    DO WHILE (ij025 < npjglo-2 )
       ij025 = ij025 + 2
       nj05 = nj05 + 1
    ENDDO
    ijend025=ij025
    PRINT *, ' NJ05 = ', nj05
    PRINT *, ' Corners of the domain :'
    PRINT *, "eORCA05(1,1) = eORCA025(",iistart025,",",ijstart025,")"
    PRINT *, "eORCA05(",ni05,",",nj05,") = eORCA025(",iiend025,",",ijend025,")"

    ! add extra row, column just in case
    nidim5=ni05+1 ; njdim5=nj05+1
    ! Allocate eORCA05 arrays
    ALLOCATE ( dphit5(nidim5,njdim5), dlamt5(nidim5,njdim5), de1t5(nidim5,njdim5), de2t5(nidim5,njdim5) )
    ALLOCATE ( dphiu5(nidim5,njdim5), dlamu5(nidim5,njdim5), de1u5(nidim5,njdim5), de2u5(nidim5,njdim5) )
    ALLOCATE ( dphiv5(nidim5,njdim5), dlamv5(nidim5,njdim5), de1v5(nidim5,njdim5), de2v5(nidim5,njdim5) )
    ALLOCATE ( dphif5(nidim5,njdim5), dlamf5(nidim5,njdim5), de1f5(nidim5,njdim5), de2f5(nidim5,njdim5) )
  END SUBROUTINE GetSize05

  SUBROUTINE BuildLonLat
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BuildLonLat  ***
    !!
    !! ** Purpose :  Build the position of eORCA05 points on the C-grid
    !!               from eORCA025 T points. North pole folding is incorrect
    !!
    !! ** Method  :  Figure out how the ORCA025 and ORCA05 grids match.
    !!
    !!----------------------------------------------------------------------
    ij=ijstart025
    DO jj=1, nj05
       ii=iistart025
       DO ji = 1, ni05
          dphit5(ji,jj) = dphit(ii  ,ij  )
          dlamt5(ji,jj) = dlamt(ii  ,ij  )

          dphiu5(ji,jj) = dphit(ii+1,ij  )
          dlamu5(ji,jj) = dlamt(ii+1,ij  )

          dphiv5(ji,jj) = dphit(ii  ,ij+1)
          dlamv5(ji,jj) = dlamt(ii  ,ij+1)

          dphif5(ji,jj) = dphit(ii+1,ij+1)
          dlamf5(ji,jj) = dlamt(ii+1,ij+1)
          ii=ii+2
       ENDDO
       ij=ij+2
    ENDDO
    ! periodicity
    dphit5(ni05+1,:) = dphit5(2,:)
    dlamt5(ni05+1,:) = dlamt5(2,:)

    dphiu5(ni05+1,:) = dphiu5(2,:)
    dlamu5(ni05+1,:) = dlamu5(2,:)

    dphiv5(ni05+1,:) = dphiv5(2,:)
    dlamv5(ni05+1,:) = dlamv5(2,:)

    dphif5(ni05+1,:) = dphif5(2,:)
    dlamf5(ni05+1,:) = dlamf5(2,:)

    ! top NE corner ??
    dphit5(ni05+1,nj05+1) = dphit(npiglo,npjglo)
    dlamt5(ni05+1,nj05+1) = dlamt(npiglo,npjglo)

    dphiu5(ni05+1,nj05+1) = dphiu(npiglo,npjglo)
    dlamu5(ni05+1,nj05+1) = dlamu(npiglo,npjglo)

    dphiv5(ni05+1,nj05+1) = dphiv(npiglo,npjglo)
    dlamv5(ni05+1,nj05+1) = dlamv(npiglo,npjglo)

    dphif5(ni05+1,nj05+1) = dphif(npiglo,npjglo)
    dlamf5(ni05+1,nj05+1) = dlamf(npiglo,npjglo)

    !top row (will not be used)
    dphit5(:,nj05+1) = dphit5(:,nj05)
    dlamt5(:,nj05+1) = dlamt5(:,nj05)

    dphiu5(:,nj05+1) = dphiu5(:,nj05)
    dlamu5(:,nj05+1) = dlamu5(:,nj05)

    dphiv5(:,nj05+1) = dphiv5(:,nj05)
    dlamv5(:,nj05+1) = dlamv5(:,nj05)

    dphif5(:,nj05+1) = dphif5(:,nj05)
    dlamf5(:,nj05+1) = dlamf5(:,nj05)

  END SUBROUTINE BuildLonLat

  SUBROUTINE BuildMetric
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE BuildMetric  ***
    !!
    !! ** Purpose :  Build de1, de2 for T U V F points on the C grid 
    !!
    !! ** Method  :  Use the Orthodromic distance on the sphere between
    !!               grid points 
    !!
    !!----------------------------------------------------------------------
    DO jj=2,nj05
      DO ji=2,ni05
        ! de1 T = dist between U(i) and U(i-1)
        ! de2 T = dist between V(j) and V(j-1)
        de1t5(ji,jj) =  dist ( dlamu5(ji,jj), dlamu5(ji-1,jj  ), dphiu5(ji,jj), dphiu5(ji-1,jj  ) ) * 1000.d0 ! in meters
        de2t5(ji,jj) =  dist ( dlamv5(ji,jj), dlamv5(ji  ,jj-1), dphiv5(ji,jj), dphiv5(ji  ,jj-1) ) * 1000.d0 ! in meters

        ! de1 U = dist between T(i) and T(i+1)
        ! de2 U = dist between F(j) and F(j-1)
        de1u5(ji,jj) =  dist ( dlamt5(ji,jj), dlamt5(ji+1,jj  ), dphit5(ji,jj), dphit5(ji+1,jj  ) ) * 1000.d0 ! in meters
        de2u5(ji,jj) =  dist ( dlamf5(ji,jj), dlamf5(ji  ,jj-1), dphif5(ji,jj), dphiv5(ji  ,jj-1) ) * 1000.d0 ! in meters

        ! de1 V = dist between F(i) and F(i-1)
        ! de2 V = dist between T(j) and T(j+1)
        de1v5(ji,jj) =  dist ( dlamf5(ji,jj), dlamf5(ji-1,jj  ), dphif5(ji,jj), dphiv5(ji-1,jj  ) ) * 1000.d0 ! in meters
        de2v5(ji,jj) =  dist ( dlamt5(ji,jj), dlamt5(ji  ,jj+1), dphit5(ji,jj), dphit5(ji  ,jj+1) ) * 1000.d0 ! in meters

        ! de1 F = dist between V(i) and V(i+1)
        ! de2 F = dist between U(j) and U(j+1)
        de1f5(ji,jj) =  dist ( dlamv5(ji,jj), dlamv5(ji+1,jj  ), dphiv5(ji,jj), dphiv5(ji+1,jj  ) ) * 1000.d0 ! in meters
        de2f5(ji,jj) =  dist ( dlamu5(ji,jj), dlamu5(ji  ,jj+1), dphiu5(ji,jj), dphiu5(ji  ,jj+1) ) * 1000.d0 ! in meters
      ENDDO
    ENDDO
    ! Periodicity
      de1t5(ni05+1,:) = de1t5(2,:)
      de1u5(ni05+1,:) = de1u5(2,:)
      de1v5(ni05+1,:) = de1v5(2,:)
      de1f5(ni05+1,:) = de1f5(2,:)

      de2t5(ni05+1,:) = de2t5(2,:)
      de2u5(ni05+1,:) = de2u5(2,:)
      de2v5(ni05+1,:) = de2v5(2,:)
      de2f5(ni05+1,:) = de2f5(2,:)

      de1t5(1,:) = de1t5(ni05,:)
      de1u5(1,:) = de1u5(ni05,:)
      de1v5(1,:) = de1v5(ni05,:)
      de1f5(1,:) = de1f5(ni05,:)

      de2t5(1,:) = de2t5(ni05,:)
      de2u5(1,:) = de2u5(ni05,:)
      de2v5(1,:) = de2v5(ni05,:)
      de2f5(1,:) = de2f5(ni05,:)
    ! first row
      de1t5(:,1)      = de1t5(:,2)
      de1u5(:,1)      = de1u5(:,2)
      de1v5(:,1)      = de1v5(:,2)
      de1f5(:,1)      = de1f5(:,2)

      de2t5(:,1)      = de2t5(:,2)
      de2u5(:,1)      = de2u5(:,2)
      de2v5(:,1)      = de2v5(:,2)
      de2f5(:,1)      = de2f5(:,2)
    ! last row
      de1t5(:,nj05+1)      = de1t5(:,nj05)
      de1u5(:,nj05+1)      = de1u5(:,nj05)
      de1v5(:,nj05+1)      = de1v5(:,nj05)
      de1f5(:,nj05+1)      = de1f5(:,nj05)

      de2t5(:,nj05+1)      = de2t5(:,nj05)
      de2u5(:,nj05+1)      = de2u5(:,nj05)
      de2v5(:,nj05+1)      = de2v5(:,nj05)
      de2f5(:,nj05+1)      = de2f5(:,nj05)
     ! corners SW
      de1t5(1,1)=de1t5(2,2)
      de1u5(1,1)=de1u5(2,2)
      de1v5(1,1)=de1v5(2,2)
      de1f5(1,1)=de1f5(2,2)

      de2t5(1,1)=de2t5(2,2)
      de2u5(1,1)=de2u5(2,2)
      de2v5(1,1)=de2v5(2,2)
      de2f5(1,1)=de2f5(2,2)
      ! NW
      de1t5(1,nj05+1)=de1t5(2,nj05)
      de1u5(1,nj05+1)=de1u5(2,nj05)
      de1v5(1,nj05+1)=de1v5(2,nj05)
      de1f5(1,nj05+1)=de1f5(2,nj05)

      de2t5(1,nj05+1)=de2t5(2,nj05)
      de2u5(1,nj05+1)=de2u5(2,nj05)
      de2v5(1,nj05+1)=de2v5(2,nj05)
      de2f5(1,nj05+1)=de2f5(2,nj05)
      ! NE
      de1t5(ni05+1,nj05+1)=de1t5(ni05,nj05)
      de1u5(ni05+1,nj05+1)=de1u5(ni05,nj05)
      de1v5(ni05+1,nj05+1)=de1v5(ni05,nj05)
      de1f5(ni05+1,nj05+1)=de1f5(ni05,nj05)

      de2t5(ni05+1,nj05+1)=de2t5(ni05,nj05)
      de2u5(ni05+1,nj05+1)=de2u5(ni05,nj05)
      de2v5(ni05+1,nj05+1)=de2v5(ni05,nj05)
      de2f5(ni05+1,nj05+1)=de2f5(ni05,nj05)
      ! SE
      de1t5(ni05+1,1)=de1t5(ni05,2)
      de1u5(ni05+1,1)=de1u5(ni05,2)
      de1v5(ni05+1,1)=de1v5(ni05,2)
      de1f5(ni05+1,1)=de1f5(ni05,2)

      de2t5(ni05+1,1)=de2t5(ni05,2)
      de2u5(ni05+1,1)=de2u5(ni05,2)
      de2v5(ni05+1,1)=de2v5(ni05,2)
      de2f5(ni05+1,1)=de2f5(ni05,2)

      WHERE (de1t5 > 60000.d0 ) de1t5=60000.d0
      WHERE (de1u5 > 60000.d0 ) de1u5=60000.d0
      WHERE (de1v5 > 60000.d0 ) de1v5=60000.d0
      WHERE (de1f5 > 60000.d0 ) de1f5=60000.d0

      WHERE (de2t5 > 60000.d0 ) de2t5=60000.d0
      WHERE (de2u5 > 60000.d0 ) de2u5=60000.d0
      WHERE (de2v5 > 60000.d0 ) de2v5=60000.d0
      WHERE (de2f5 > 60000.d0 ) de2f5=60000.d0


  END SUBROUTINE BuildMetric

  SUBROUTINE Merge05
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Merge05  ***
    !!
    !! ** Purpose :   patch northern part of the domain from ORCA05
    !!                to freshly build eORCA05
    !!
    !! ** Method  :   patch north of a common line (hand defined)
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4) :: ijcommon1= 79   ! ORCA05
    INTEGER(KIND=4) :: ijcommon2=172   ! eORCA05
    !!----------------------------------------------------------------------
    IF ( nidim5 /= npiglo ) THEN
      PRINT *, ' PROBLEM : I grid sizes mismatch !'
      STOP 1
    ENDIF
    ij=ijcommon2
    DO jj=ijcommon1,npjglo
       dphit5(:,ij) = dphit(:,jj)
       dlamt5(:,ij) = dlamt(:,jj)
       de1t5(:,ij)  = de1t(:,jj)
       de2t5(:,ij)  = de2t(:,jj)

       dphiu5(:,ij) = dphiu(:,jj)
       dlamu5(:,ij) = dlamu(:,jj)
       de1u5(:,ij)  = de1u(:,jj)
       de2u5(:,ij)  = de2u(:,jj)

       dphiv5(:,ij) = dphiv(:,jj)
       dlamv5(:,ij) = dlamv(:,jj)
       de1v5(:,ij)  = de1v(:,jj)
       de2v5(:,ij)  = de2v(:,jj)

       dphif5(:,ij) = dphif(:,jj)
       dlamf5(:,ij) = dlamf(:,jj)
       de1f5(:,ij)  = de1f(:,jj)
       de2f5(:,ij)  = de2f(:,jj)
       ij=ij+1
    ENDDO
    ! update njdim5 to real value
    njdim5=ij - 1
    

  END SUBROUTINE Merge05
  SUBROUTINE Wcoord
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE Wcoord  ***
    !!
    !! ** Purpose :  write eORCA05 guess ( useful for the southern part) 
    !!
    !! ** Method  :  Netcdf primitive functions 
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4) :: idx, idy, idt
    INTEGER(KIND=4) :: idlon, idlat, idtim
    INTEGER(KIND=4) :: idphit, idlamt, ide1t, ide2t
    INTEGER(KIND=4) :: idphiu, idlamu, ide1u, ide2u
    INTEGER(KIND=4) :: idphiv, idlamv, ide1v, ide2v
    INTEGER(KIND=4) :: idphif, idlamf, ide1f, ide2f

    ierr = NF90_CREATE( cf_coord05, NF90_NETCDF4, ncid)
    ! definition of dimension
    ierr = NF90_DEF_DIM(ncid,'x',nidim5,idx)
    ierr = NF90_DEF_DIM(ncid,'y',njdim5,idy)
    ierr = NF90_DEF_DIM(ncid,'time_counter',NF90_UNLIMITED,idt)
    ! definition of variables
    ierr = NF90_DEF_VAR(ncid,'nav_lon', NF90_FLOAT, (/idx,idy/), idlon)
    ierr = NF90_DEF_VAR(ncid,'nav_lat', NF90_FLOAT, (/idx,idy/), idlat)
    ierr = NF90_DEF_VAR(ncid,'time_counter', NF90_DOUBLE, (/idt/), idtim)

    ! T points
    ierr = NF90_DEF_VAR(ncid,'gphit',NF90_DOUBLE,(/idx,idy,idt/), idphit )
    ierr = NF90_DEF_VAR(ncid,'glamt',NF90_DOUBLE,(/idx,idy,idt/), idlamt )
    ierr = NF90_DEF_VAR(ncid,'e1t',  NF90_DOUBLE,(/idx,idy,idt/), ide1t )
    ierr = NF90_DEF_VAR(ncid,'e2t',  NF90_DOUBLE,(/idx,idy,idt/), ide2t )

    ! U points
    ierr = NF90_DEF_VAR(ncid,'gphiu',NF90_DOUBLE,(/idx,idy,idt/), idphiu )
    ierr = NF90_DEF_VAR(ncid,'glamu',NF90_DOUBLE,(/idx,idy,idt/), idlamu )
    ierr = NF90_DEF_VAR(ncid,'e1u',  NF90_DOUBLE,(/idx,idy,idt/), ide1u )
    ierr = NF90_DEF_VAR(ncid,'e2u',  NF90_DOUBLE,(/idx,idy,idt/), ide2u )

    ! V points
    ierr = NF90_DEF_VAR(ncid,'gphiv',NF90_DOUBLE,(/idx,idy,idt/), idphiv )
    ierr = NF90_DEF_VAR(ncid,'glamv',NF90_DOUBLE,(/idx,idy,idt/), idlamv )
    ierr = NF90_DEF_VAR(ncid,'e1v',  NF90_DOUBLE,(/idx,idy,idt/), ide1v )
    ierr = NF90_DEF_VAR(ncid,'e2v',  NF90_DOUBLE,(/idx,idy,idt/), ide2v )

    ! F points
    ierr = NF90_DEF_VAR(ncid,'gphif',NF90_DOUBLE,(/idx,idy,idt/), idphif )
    ierr = NF90_DEF_VAR(ncid,'glamf',NF90_DOUBLE,(/idx,idy,idt/), idlamf )
    ierr = NF90_DEF_VAR(ncid,'e1f',  NF90_DOUBLE,(/idx,idy,idt/), ide1f )
    ierr = NF90_DEF_VAR(ncid,'e2f',  NF90_DOUBLE,(/idx,idy,idt/), ide2f )

    ierr = NF90_ENDDEF(ncid)
    ! Put variables
    ierr = NF90_PUT_VAR(ncid, idlon, dlamt5, start=(/1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, idlat, dphit5, start=(/1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, idtim, (/0.d0/) )

    ierr = NF90_PUT_VAR(ncid, idphit, dphit5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, idlamt, dlamt5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )

    ierr = NF90_PUT_VAR(ncid, idphiu, dphiu5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, idlamu, dlamu5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )

    ierr = NF90_PUT_VAR(ncid, idphiv, dphiv5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, idlamv, dlamv5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )

    ierr = NF90_PUT_VAR(ncid, idphif, dphif5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, idlamf, dlamf5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )

    ! metrics
    ierr = NF90_PUT_VAR(ncid, ide1t, de1t5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, ide2t, de2t5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )

    ierr = NF90_PUT_VAR(ncid, ide1u, de1u5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, ide2u, de2u5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )

    ierr = NF90_PUT_VAR(ncid, ide1v, de1v5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, ide2v, de2v5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )

    ierr = NF90_PUT_VAR(ncid, ide1f, de1f5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )
    ierr = NF90_PUT_VAR(ncid, ide2f, de2f5, start=(/1,1,1/), count=(/nidim5,njdim5,1/) )


    ierr = NF90_CLOSE(ncid)

  END SUBROUTINE Wcoord

  REAL(KIND=8) FUNCTION dist(ddlona, ddlonb, ddlata, ddlatb)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION dist  ***
    !!
    !! ** Purpose : Compute the distance (km) between
    !!              point A (lona, lata) and B (lonb, latb)  
    !!
    !! ** Method  : Use of double precision is important. Compute the 
    !!              distance along the orthodromy
    !!
    !!----------------------------------------------------------------------
    REAL(KIND=8), INTENT(in) :: ddlata, ddlona, ddlatb, ddlonb

    REAL(KIND=8), SAVE :: dl_latar, dl_latbr, dl_lonar, dl_lonbr
    REAL(KIND=8)       :: dl_pds
    REAL(KIND=8), SAVE :: dl_ux, dl_uy, dl_uz
    REAL(KIND=8)       :: dl_vx, dl_vy, dl_vz
    REAL(KIND=8), SAVE :: dl_prevlat=-1000.d0
    REAL(KIND=8), SAVE :: dl_prevlon=-1000.d0
    REAL(KIND=8), SAVE :: dl_r, dl_pi, dl_conv

    LOGICAL :: ll_first=.TRUE.
    !!----------------------------------------------------------------------
    ! initialise some values at first call
    IF ( ll_first ) THEN
       ll_first = .FALSE.
       ! constants
       dl_pi   = ACOS(-1.d0)
       dl_conv = dl_pi/180.d0  ! for degree to radian conversion
       ! Earth radius
       dl_r    = (6378.137d0+6356.7523d0)/2.0d0 ! km
    ENDIF

    ! compute these term only if they differ from previous call
    IF ( ddlata /= dl_prevlat .OR. ddlona /= dl_prevlon) THEN
       dl_latar   = ddlata*dl_conv
       dl_lonar   = ddlona*dl_conv
       dl_ux      = COS(dl_lonar)*COS(dl_latar)
       dl_uy      = SIN(dl_lonar)*COS(dl_latar)
       dl_uz      = SIN(dl_latar)
       dl_prevlat = ddlata
       dl_prevlon = ddlona
    ENDIF

    dl_latbr = ddlatb*dl_conv
    dl_lonbr = ddlonb*dl_conv
    dl_vx    = COS(dl_lonbr)*COS(dl_latbr)
    dl_vy    = SIN(dl_lonbr)*COS(dl_latbr)
    dl_vz    = SIN(dl_latbr)

    dl_pds   = dl_ux*dl_vx + dl_uy*dl_vy + dl_uz*dl_vz

    IF (dl_pds >= 1.) THEN
       dist = 0.
    ELSE
       dist = dl_r*ACOS(dl_pds)
    ENDIF

  END FUNCTION dist
END PROGRAM mkorca05
