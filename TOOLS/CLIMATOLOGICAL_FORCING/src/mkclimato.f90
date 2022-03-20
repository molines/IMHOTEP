PROGRAM mkclimato
!!!----------------------------------------------------------------------------
!!!                           ***  PROGRAM mkclimato  ***
!!!  * Purpose : compute climatological forcing fields from atm grid files (ERA40 format)
!!!  * method : 
!!!   history : original : J.M. Molines Sep. 2008
!!!   history : generalization for any forcing file : J.M. Molines June 2014
!!!----------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  ! Parameters to be adjusted according to input files
  INTEGER, PARAMETER :: jp_npd=8    ! number of point per day  ! 4 = ERA40, 8 = ERAinterim
  CHARACTER(LEN=80), PARAMETER :: cp_lonname='lon', cp_latname='lat', cp_timname='time'
  !----------------------------------------------------------------------------------------
  ! command line stuff
  INTEGER :: narg, iargc

  INTEGER :: ji,jj,jfich, jv, jt, jat
  INTEGER :: npiglo,npjglo, nt, nvar, natt
  INTEGER :: idum, i1,i2, ipos, iy1, iy2
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlon, rlat, rtime
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: var
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: varday
  REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: varsum
  REAL(KIND=4) :: spval = -9999.
  REAL(KIND=4) :: rsf, rao

  CHARACTER(LEN=80) :: cfile, cvar, cfileout='climato.nc',cfileoutf='climato-f.nc', catt, cdum
  ! filtering stuff
  INTEGER :: nspan
  INTEGER, DIMENSION(:), ALLOCATABLE :: iw
  REAL(KIND=4) :: tcoup, dt, fn
  REAL(KIND=4)                           ::  zpi,zden,zyy,zey, zcoef, zcoef2
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: tmp, tmpf
  REAL(KIND=8), DIMENSION(:), ALLOCATABLE ::  zec,ze

  ! netcdf stuff
  INTEGER :: istatus, ncid, ncout, ncoutf
  INTEGER :: id_lon, id_lat, id_dlon, id_dlat, id_var, id_varo, id_varof
  INTEGER :: id_dtime, id_time
  INTEGER :: ierr1, ierr2
  !----------------------------------------------------------------------------------------

  ! parse command line
  narg=iargc()

  IF ( narg == 0 ) THEN
     PRINT *,' usage :  mkclimato ''list of data files'' '
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       Compute a daily climatology of the input files. Input files are'
     PRINT *,'       likely provided at higher frequency. Number of time frames per day'
     PRINT *,'       must be adjusted at compile time, parameter jp_npd'
     PRINT '("        Actual value is ",i2," corresponding to ",i2,"-hourly files" )', jp_npd, 24/jp_npd
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'        liste of data file for time averaging' 
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'        none'
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       none' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file : climato.nc : climatological file (365 days)'
     PRINT *,'                     climato-f.nc : filtered climatological file (365 days)'
     PRINT *,'                        default filtering is Lanczos filterat 10 days.'
     PRINT *,'         variables : same as in input files'
     PRINT *,'      '
     PRINT *,'     SEE ALSO :'
     PRINT *,'       mkclimato  mkclimato_daily  mkmodxu  mkonlyfilter  mkonlyhanning mkw10'
     PRINT *,'      '
     STOP
  ENDIF
  CALL getarg(1   ,cfile)
  CALL getarg(narg, cdum)
  ! try to infer first and last year of climatology <prefix>_<var>_<dataset>_y<year>.nc
  ipos=index(cfile,'_',.true.)  ! position of last _ in cfile 
  READ(cfile(ipos+2:ipos+5),* ) iy1
  ipos=index(cdum,'_',.true.)  ! position of last _ in cfile 
  READ(cdum(ipos+2:ipos+5),* ) iy2
  
  

  ! init 10 days lancsos filter
  tcoup=10*24.  !  10 days in hours
  dt=24.         ! hours filter is applied on daily fields
  fn=dt/tcoup ! tcoup/dt is the number of data point during tcoup. fn is just the inverse.
  nspan=NINT(2./fn)
  ! *  weight coefficients
  ALLOCATE (zec(0:nspan), ze(0:nspan ))
  CALL initlanc(nspan,fn)

  ! CDF stuff in the code
  istatus= NF90_OPEN(cfile,NF90_NOWRITE,ncid)
  ! get lon lat time dimension 
  ! assume all file identical
  istatus=NF90_INQ_DIMID(ncid,cp_lonname,id_dlon)
  istatus=NF90_INQ_DIMID(ncid,cp_latname ,id_dlat)
  istatus=NF90_INQ_DIMID(ncid,cp_timname ,id_dtime)

  istatus=NF90_INQUIRE_DIMENSION(ncid,id_dlon,len=npiglo)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_dlat,len=npjglo)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_dtime,len=nt)

  ! look for variable name in file (the only XYT var in file)
  istatus=NF90_INQUIRE(ncid,nVariables=nvar)
  DO jv=1,nvar
     istatus=NF90_INQUIRE_VARIABLE(ncid,jv,name=cvar,ndims=idum)
     IF ( idum == 3 ) EXIT
  END DO
  PRINT *,' working for variable ', TRIM(cvar),' with ', narg,' files'


  PRINT *,' ALLOCATE space ...'
  ALLOCATE ( rlon(npiglo), rlat(npjglo), var(npiglo,npjglo), varsum(npiglo,npjglo,nt) )
  ALLOCATE ( rtime(nt/jp_npd) ,varday(npiglo,npjglo,nt/jp_npd) )
  ALLOCATE (tmp(-nspan+1:nt/jp_npd+nspan), tmpf(-nspan+1:nt/jp_npd+nspan) )
  ALLOCATE (iw(-nspan+1:nt/jp_npd+nspan) )
  iw(:)=1
  PRINT *,' ALLOCATE space done'
  ! get lon lat array, once for all
  istatus=NF90_INQ_VARID(ncid,cp_lonname,id_var)
  istatus=NF90_GET_VAR(ncid,id_var,rlon(:),start=(/1/), count=(/npiglo/) )
  istatus=NF90_INQ_VARID(ncid,cp_latname ,id_var)
  istatus=NF90_GET_VAR(ncid,id_var,rlat(:),start=(/1/), count=(/npjglo/) )
  rtime(:)=(/(FLOAT(jt),jt=1,nt/jp_npd)/)

  ! output on climato.nc
  istatus=NF90_CREATE(cfileout,NF90_CLOBBER,ncout)
  ! define dims
  istatus=NF90_DEF_DIM(ncout,cp_lonname,npiglo,id_dlon)
  istatus=NF90_DEF_DIM(ncout,cp_latname ,npjglo,id_dlat)
  istatus=NF90_DEF_DIM(ncout,cp_timname ,NF90_UNLIMITED,id_dtime)
  ! define var
  istatus=NF90_DEF_VAR(ncout,cp_lonname,NF90_FLOAT,(/id_dlon/),id_lon)
  istatus=NF90_DEF_VAR(ncout,cp_latname ,NF90_FLOAT,(/id_dlat/),id_lat)
  istatus=NF90_DEF_VAR(ncout,cp_timname ,NF90_FLOAT,(/id_dtime/),id_time)
  istatus=NF90_DEF_VAR(ncout,cvar,NF90_FLOAT,(/id_dlon,id_dlat,id_dtime/),id_varo)
  ! attributes of variable
  istatus=NF90_INQ_VARID(ncid,cvar,id_var)
  ! RD : makes trouble with add_offset / scale factor  
  istatus=NF90_INQUIRE_VARIABLE(ncid,id_var,nAtts=natt)
  DO jat=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,id_var,jat,catt)
     IF ( catt /= 'add_offset' .AND. catt /= 'scale_factor' ) THEN
       istatus=NF90_COPY_ATT(ncid,id_var,catt,ncout,id_varo)
     ENDIF
  ENDDO
  WRITE(cdum,'("Daily climatology ",i4,"-",i4)') iy1,iy2
  istatus=NF90_PUT_ATT(ncout, NF90_GLOBAL, 'history', cdum )
  istatus=NF90_ENDDEF(ncout)
  ! copy lon lat and time in the output file
  istatus=NF90_PUT_VAR(ncout,id_lon,rlon)
  istatus=NF90_PUT_VAR(ncout,id_lat,rlat)
  istatus=NF90_PUT_VAR(ncout,id_time,rtime)

  ! idem for filtered fields

  istatus=NF90_CREATE(cfileoutf,NF90_CLOBBER,ncoutf)
  ! define dims
  istatus=NF90_DEF_DIM(ncoutf,cp_lonname,npiglo,id_dlon)
  istatus=NF90_DEF_DIM(ncoutf,cp_latname ,npjglo,id_dlat)
  istatus=NF90_DEF_DIM(ncoutf,cp_timname ,NF90_UNLIMITED,id_dtime)
  ! define var
  istatus=NF90_DEF_VAR(ncoutf,cp_lonname,NF90_FLOAT,(/id_dlon/),id_lon)
  istatus=NF90_DEF_VAR(ncoutf,cp_latname ,NF90_FLOAT,(/id_dlat/),id_lat)
  istatus=NF90_DEF_VAR(ncoutf,cp_timname ,NF90_FLOAT,(/id_dtime/),id_time)
  istatus=NF90_DEF_VAR(ncoutf,cvar,NF90_FLOAT,(/id_dlon,id_dlat,id_dtime/),id_varof)
  ! attributes of variable
  istatus=NF90_INQ_VARID(ncid,cvar,id_var)

  ! RD : makes trouble with add_offset / scale factor 
  istatus=NF90_INQUIRE_VARIABLE(ncid,id_var,nAtts=natt)
  DO jat=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,id_var,jat,catt)
     IF ( catt /= 'add_offset' .AND. catt /= 'scale_factor' ) THEN
       istatus=NF90_COPY_ATT(ncid,id_var,catt,ncoutf,id_varof)
     ENDIF
  ENDDO
  WRITE(cdum,'("Daily climatology ",i4,"-",i4," Lanczos filter 10days")') iy1,iy2
  istatus=NF90_PUT_ATT(ncoutf, NF90_GLOBAL, 'history', cdum )
  istatus=NF90_ENDDEF(ncoutf)
  ! copy lon lat and time in the output file
  istatus=NF90_PUT_VAR(ncoutf,id_lon,rlon)
  istatus=NF90_PUT_VAR(ncoutf,id_lat,rlat)
  istatus=NF90_PUT_VAR(ncoutf,id_time,rtime)


  istatus=NF90_CLOSE(ncid)

  varsum(:,:,:) = 0.d0
  DO jfich=1, narg
     CALL getarg(jfich,cfile)
     PRINT *,' working with file #',jfich,' :', TRIM(cfile)
     istatus= NF90_OPEN(cfile,NF90_NOWRITE,ncid)
     istatus=NF90_INQ_VARID(ncid,cvar,id_var)
     ! RD : strange things happen to varsum
     !istatus=NF90_GET_ATT(ncid,id_var,'missing_value',spval)

     ierr1 = NF90_GET_ATT(ncid, id_var, 'scale_factor', rsf)
     ierr2 = NF90_GET_ATT(ncid, id_var, 'add_offset',   rao)

     IF ( (ierr1 /= NF90_NOERR).OR.(ierr2 /= NF90_NOERR) ) THEN
       rsf = 1.      ;   rao = 0.
     ENDIF

     PRINT *, 'file', TRIM(cfile),' : offset = ' , rao , 'scale =', rsf
     PRINT *, 'spval = ', spval

     DO jt=1,nt
        istatus=NF90_GET_VAR(ncid,id_var,var(:,:),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
        var(:,:) = rsf * var(:,:) + rao
        WHERE (var /= spval) 
           varsum(:,:,jt)= varsum(:,:,jt) + var(:,:)
        ELSEWHERE
           varsum(:,:,jt)=spval
        END WHERE
     END DO
     PRINT *, '****************************************'
     PRINT *, '>>> varsum range =',MINVAL(varsum) , '-', MAXVAL(varsum)
     PRINT *, '****************************************'
     istatus=NF90_CLOSE(ncid)
  END DO

     PRINT *,' compute mean'
     DO jt=1,nt
        WHERE (var /= spval ) varsum(:,:,jt)=varsum(:,:,jt)/narg
     ENDDO
     
     PRINT *,' Compute daily fields'
     DO jt=1,nt/jp_npd
      i1=(jt-1)*jp_npd+1 ; i2=i1+jp_npd-1
      varday(:,:,jt)=0.
      DO ji=i1,i2
       varday(:,:,jt)=varsum(:,:,ji)/jp_npd +varday(:,:,jt)
      END DO
     END DO
     ! output for non filtered fields
     DO jt=1,nt/jp_npd
       istatus=NF90_PUT_VAR(ncout,id_varo,varday(:,:,jt),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
     END DO
     istatus=NF90_CLOSE(ncout)

     PRINT *, ' 10 days filtering and periodisation nspan=', nspan
     PRINT *, nt/jp_npd, nt/jp_npd+1,nt/jp_npd+nspan, -nspan+1,nt/jp_npd-nspan+1, nt/jp_npd+2*nspan
     DO ji=1,npiglo
       DO jj=1,npjglo
           tmp=0.
           IF ( varday(ji,jj,1) /= spval ) THEN
           tmp(1:nt/jp_npd)=varday(ji,jj,1:nt/jp_npd)
           tmp(nt/jp_npd+1:nt/jp_npd+nspan)=varday(ji,jj,1:nspan)
           tmp(-nspan+1:0)=varday(ji,jj,nt/jp_npd-nspan+1:nt/jp_npd)
           CALL lislanczos(tmp,iw,tmpf,nt/jp_npd+2*nspan,fn,nspan)
           ELSE
           tmpf(:)=spval
           ENDIF
           varday(ji,jj,1:nt/jp_npd)=tmpf(1:nt/jp_npd)
       ENDDO
     ENDDO

     PRINT *, 'Final output of filtered, periodized fields'
     DO jt=1,nt/jp_npd
       istatus=NF90_PUT_VAR(ncoutf,id_varof,varday(:,:,jt),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
     END DO
     istatus=NF90_CLOSE(ncoutf)


CONTAINS
  SUBROUTINE initlanc(knj, pfn)
    !! ---------------------------------------------------------------------------
    !!          ***  SUBROUTINE initlanc  ***
    !!  
    !! ** Purpose: init lancsos coeficient
    !!
    !! ---------------------------------------------------------------------------
    INTEGER, INTENT(in) :: knj
    REAL(KIND=4), INTENT(in):: pfn
   
    zpi=ACOS(-1.)
    ze(0)=2.*pfn
    zcoef=2.*pfn*zpi ; zcoef2=zpi/knj

    DO  ji=1,knj
       ze(ji)=SIN(zcoef*ji)/(zpi*ji)
    END DO
    !
    zec(0)=2.*pfn
    DO jj=1,knj
       zey=zcoef2*jj
       zec(jj)=ze(jj)*SIN(zey)/zey
    END DO
    END SUBROUTINE initlanc

  SUBROUTINE lislanczos(px,kiw,py,kn,pfn,knj)
    !! ---------------------------------------------------------------------------
    !!           ***   SUBROUTINE lislanczos   ***
    !!
    !!   ** Purpose :  apply lanczos filter to input vector
    !!
    !!   ** Method  :  lanczos weight are computed
    !!               then convolution is done
    !      x=input data
    !      kiw = validity of input data
    !      y=output filter
    !      kn=number of input/output data
    !      knj= bandwith of the filter
    !      pfn= cutoff frequency
    !! * history
    !!      Eric Blayo d'apres une source CLS fournie par F. BLANC.
    !!           et grosse(s)  optimization(s).
    !!      J.M. Molines : Dr Norm : 11/03/2002 19:35
    !!      J.M. Molines : F90 : 31/05/2007 20:29
    !!---------------------------------------------------------------------------
    ! * Arguments
    REAL(KIND=4), DIMENSION(:), INTENT(in)  :: px  !: Input time series
    REAL(KIND=4), DIMENSION(:), INTENT(out) :: py  !: output filtered time series
    REAL(KIND=4),               INTENT(in)  :: pfn !: cutoff freq.
    INTEGER,      DIMENSION(:), INTENT(in)  :: kiw !: flag for missing data 1=good, 0= bad
    INTEGER,                    INTENT(in)  :: kn  !: number of points in px and py
    INTEGER,                    INTENT(in)  :: knj !: band width of the filter

    ! * Local variables
    INTEGER  :: ji,jj,jm,jk,kk
    INTEGER  :: nmin,nmax,k1,k2
    !

    ! * Filtering
    nmin=knj
    nmax=kn-knj+1
    !
    DO jm=1,kn
       k1=-knj
       k2=knj
       IF (jm <= nmin) k1=1-jm
       IF (jm >= nmax) k2=kn-jm
       !
       zyy=0.
       zden=0.
       !
       DO jk=k1,k2
          kk=ABS(jk)
          IF (kiw(jk+jm) == 1) THEN
             zden=zden+zec(kk)
             zyy=zyy+zec(kk)*px(jk+jm)
          END IF
       END DO
       IF (zden /= 0) THEN
          py(jm)=zyy/zden
       ELSE
          py(jm)=999.0
       END IF
    END DO
  END SUBROUTINE lislanczos

   END PROGRAM mkclimato
