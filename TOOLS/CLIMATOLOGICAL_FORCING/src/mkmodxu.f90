PROGRAM mkmodxu
!!!----------------------------------------------------------------------------
!!!                           ***  PROGRAM mkmodxu  ***
!!!  * Purpose : compute wu10, wv10 fields from u10 v10
!!!  * method : 
!!!   history : original : J.M. Molines Sep. 2008
!!!             2012     : R. Dussin - Add more flexibility
!!!----------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  ! command line stuff
  INTEGER :: narg, iargc, ijarg

  INTEGER :: ji,jj,jfich, jv, jt, jat
  INTEGER :: npiglo,npjglo, nt, nvar, natt
  INTEGER :: idum, i1,i2
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlon, rlat, rtime
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: varu, varv, varwu, varwv,varw
  REAL(KIND=4) :: spval = -9999.
  REAL(KIND=4) :: rsfu, rsfv, raou, raov ! scale factors / offsets

  CHARACTER(LEN=80) :: cfileu, cfilev, cvar, cvaru='u10', cvarv='v10'
  CHARACTER(LEN=80) :: cfilewu, cfilewv, catt, cyear, cset
  CHARACTER(LEN=80) :: cvarwu='wu10', cvarwv='wv10'
  CHARACTER(LEN=80) :: clon='lon', clat='lat', ctime='time', cprefix=''
  CHARACTER(LEN=80) :: cldum

  ! netcdf stuff
  INTEGER :: istatus, ncidu,ncidv, ncoutwu, ncoutwv
  INTEGER :: ierr1, ierr2
  INTEGER :: id_lon, id_lat, id_dlon, id_dlat, id_var, id_varu, id_varv, id_varwu, id_varwv
  INTEGER :: id_dtime, id_time, id_varlon, id_varlat

  ! parse command line
  narg=iargc()

  IF ( narg == 0 ) THEN
     PRINT *,' usage :   mkmodxu  -y year -set ATM-dtaset [-u10 NAM-u10 -v10 NAM-v10 '
     PRINT *,'           -wu10 NAM-wu10 -wv10 NAM-wv10 -lon NAM-lon -lat NAM-lat '
     PRINT *,'            -time NAM-time -prefix PREFIX]'
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'        Takes u10 and v10 files and compute the pseudo stresses :'
     PRINT *,'        W * u10, Wi * v10, where W is the wind module (sqrt(u10^2 + v10^2) ).'
     PRINT *, '       Output files with same number of time frame.'
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'        -y year : current year to process, e.g.: y1979.'
     PRINT *,'        -set ATM-dtaset  : name of data set to process. '
     PRINT *,'               Assume files such as u10_<dataset>_<year>.nc'
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'        -lon NAM-lon: name of longitude dimension in file'
     PRINT *,'        -lat NAM-lat: name of latitude dimension in file'
     PRINT *,'        -time NAM-time: name of time dimension in file'
     PRINT *,'        -prefix PREFIX: characters to be added before variable name in filename'
     PRINT *,'                  e.g. . drowned_'
     PRINT *,'        -u10 NAM-u10 : name of u10 wind component'
     PRINT *,'        -v10 NAM-v10 : name of v10 wind component'
     PRINT *,'        -wu10 NAM-wu10 : name of wu10 (speudo stress U output)'
     PRINT *,'        -wv10 NAM-wv10 : name of wv10 (speudo stress V output)'
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       none' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf files :  umodU10_<dataset>_<year>.nc'
     PRINT *,'                       vmodU10_<dataset>_<year>.nc'
     PRINT *,'         variables : ', TRIM(cvarwu)//' and '//TRIM(cvarwv)
     PRINT *,'      '
     PRINT *,'     SEE ALSO :'
     PRINT *,'      mkclimato  mkclimato_daily  mkmodxu  mkonlyfilter  mkonlyhanning  mkw10' 
     PRINT *,'      '
     STOP
  ENDIF

  ijarg=1
  DO WHILE ( ijarg <= narg )
     CALL getarg(ijarg, cldum)  ; ijarg=ijarg+1
     SELECT CASE ( cldum)
     CASE ( '-y'    ) ; CALL getarg(ijarg,cyear) ; ijarg=ijarg+1
     CASE ( '-set'  ) ; CALL getarg(ijarg,cset ) ; ijarg=ijarg+1
          ! options
     CASE ( '-lon'  ) ; CALL getarg(ijarg,clon ) ; ijarg=ijarg+1
     CASE ( '-lat'  ) ; CALL getarg(ijarg,clat ) ; ijarg=ijarg+1
     CASE ( '-time' ) ; CALL getarg(ijarg,ctime) ; ijarg=ijarg+1
     CASE ( '-u10'  ) ; CALL getarg(ijarg,cvaru) ; ijarg=ijarg+1
     CASE ( '-v10'  ) ; CALL getarg(ijarg,cvarv) ; ijarg=ijarg+1
     CASE ( '-wu10' ) ; CALL getarg(ijarg,cvarwu); ijarg=ijarg+1
     CASE ( '-wv10' ) ; CALL getarg(ijarg,cvarwv); ijarg=ijarg+1
     CASE ( '-prefix'); CALL getarg(ijarg,cprefix) ; ijarg=ijarg+1
     CASE DEFAULT     ; PRINT *, 'E R R O R : Unknown option : ',TRIM(cldum) ; STOP
     END SELECT
  ENDDO

  cfileu=TRIM(cprefix)//TRIM(cvaru)//'_'//TRIM(cset)//'_'//TRIM(cyear)//'.nc'
  cfilev=TRIM(cprefix)//TRIM(cvarv)//'_'//TRIM(cset)//'_'//TRIM(cyear)//'.nc'
  cfilewu=TRIM(cprefix)//TRIM(cvarwu)//'_'//TRIM(cset)//'_'//TRIM(cyear)//'.nc'
  cfilewv=TRIM(cprefix)//TRIM(cvarwv)//'_'//TRIM(cset)//'_'//TRIM(cyear)//'.nc'

  ! CDF stuff in the code
  istatus= NF90_OPEN(cfileu,NF90_NOWRITE,ncidu)
  ! get lon lat time dimension 
  ! assume all file identical
  istatus=NF90_INQ_DIMID(ncidu,TRIM(clon),id_dlon)
  istatus=NF90_INQ_DIMID(ncidu,TRIM(clat),id_dlat)
  istatus=NF90_INQ_DIMID(ncidu,TRIM(ctime),id_dtime)

  istatus=NF90_INQUIRE_DIMENSION(ncidu,id_dlon,len=npiglo)
  istatus=NF90_INQUIRE_DIMENSION(ncidu,id_dlat,len=npjglo)
  istatus=NF90_INQUIRE_DIMENSION(ncidu,id_dtime,len=nt)


  PRINT *,' ALLOCATE space ...'
  ALLOCATE ( rlon(npiglo), rlat(npjglo), varu(npiglo,npjglo), varv(npiglo,npjglo),varw(npiglo,npjglo) )
  ALLOCATE ( varwu(npiglo,npjglo),varwv(npiglo,npjglo) )
  ALLOCATE ( rtime(nt) )
  PRINT *,' ALLOCATE space done'
  ! get lon lat array, once for all
  istatus=NF90_INQ_VARID(ncidu,TRIM(clon),id_varlon)
  istatus=NF90_GET_VAR(ncidu,id_varlon,rlon(:),start=(/1/), count=(/npiglo/) )
  istatus=NF90_INQ_VARID(ncidu,TRIM(clat),id_varlat)
  istatus=NF90_GET_VAR(ncidu,id_varlat,rlat(:),start=(/1/), count=(/npjglo/) )
  istatus=NF90_INQ_VARID(ncidu,TRIM(ctime),id_time)
  istatus=NF90_GET_VAR(ncidu,id_time,rtime(:),start=(/1/), count=(/nt/) )

  ! output on cfilewu
  istatus=NF90_CREATE(cfilewu,NF90_CLOBBER,ncoutwu)
  ! define dims
  istatus=NF90_DEF_DIM(ncoutwu,clon,npiglo,id_dlon)
  istatus=NF90_DEF_DIM(ncoutwu,clat,npjglo,id_dlat)
  istatus=NF90_DEF_DIM(ncoutwu,TRIM(ctime),NF90_UNLIMITED,id_dtime)
  ! define var
  istatus=NF90_DEF_VAR(ncoutwu,clon,NF90_FLOAT,(/id_dlon/),id_lon)
  istatus=NF90_DEF_VAR(ncoutwu,clat,NF90_FLOAT,(/id_dlat/),id_lat)
  istatus=NF90_DEF_VAR(ncoutwu,TRIM(ctime),NF90_FLOAT,(/id_dtime/),id_time)
  istatus=NF90_DEF_VAR(ncoutwu,cvarwu,NF90_FLOAT,(/id_dlon,id_dlat,id_dtime/),id_varwu)
  ! attributes of variable
  istatus=NF90_PUT_ATT(ncoutwu,id_varwu,'long_name',TRIM(cset)//': corrected 10m wind velocity x u10' )
  istatus=NF90_PUT_ATT(ncoutwu,id_varwu,'units','m2/s2' )
  istatus=NF90_PUT_ATT(ncoutwu,id_varwu,'missing_value',spval )
  istatus=NF90_PUT_ATT(ncoutwu,id_varwu,'valid_range',(/-2000.,2000./)  )
  ! copy existing attributes:
  istatus=NF90_COPY_ATT(ncidu, id_varlon,'units', ncoutwu, id_lon)
  istatus=NF90_COPY_ATT(ncidu, id_varlon,'valid_min', ncoutwu, id_lon)
  istatus=NF90_COPY_ATT(ncidu, id_varlon,'valid_max', ncoutwu, id_lon)

  istatus=NF90_COPY_ATT(ncidu, id_varlat,'units', ncoutwu, id_lat)
  istatus=NF90_COPY_ATT(ncidu, id_varlat,'valid_min', ncoutwu, id_lat)
  istatus=NF90_COPY_ATT(ncidu, id_varlat,'valid_max', ncoutwu, id_lat)

  istatus=NF90_ENDDEF(ncoutwu)
  ! copy lon lat and time in the outwu file
  istatus=NF90_PUT_VAR(ncoutwu,id_lon,rlon)
  istatus=NF90_PUT_VAR(ncoutwu,id_lat,rlat)
  istatus=NF90_PUT_VAR(ncoutwu,id_time,rtime)

  ! output on cfilewv
  istatus=NF90_CREATE(cfilewv,NF90_CLOBBER,ncoutwv)
  ! define dims
  istatus=NF90_DEF_DIM(ncoutwv,clon,npiglo,id_dlon)
  istatus=NF90_DEF_DIM(ncoutwv,clat,npjglo,id_dlat)
  istatus=NF90_DEF_DIM(ncoutwv,TRIM(ctime),NF90_UNLIMITED,id_dtime)
  ! define var
  istatus=NF90_DEF_VAR(ncoutwv,clon,NF90_FLOAT,(/id_dlon/),id_lon)
  istatus=NF90_DEF_VAR(ncoutwv,clat,NF90_FLOAT,(/id_dlat/),id_lat)
  istatus=NF90_DEF_VAR(ncoutwv,TRIM(ctime),NF90_FLOAT,(/id_dtime/),id_time)
  istatus=NF90_DEF_VAR(ncoutwv,cvarwv,NF90_FLOAT,(/id_dlon,id_dlat,id_dtime/),id_varwv)
  ! attributes of variable
  istatus=NF90_PUT_ATT(ncoutwv,id_varwv,'long_name',TRIM(cset)//': corrected ERA40, 10m wind velocity x v10' )
  istatus=NF90_PUT_ATT(ncoutwv,id_varwv,'units','m2/s2' )
  istatus=NF90_PUT_ATT(ncoutwv,id_varwv,'missing_value',spval )
  istatus=NF90_PUT_ATT(ncoutwv,id_varwv,'valid_range',(/-2000.,2000./)  )
  ! copy existing attributes:
  istatus=NF90_COPY_ATT(ncidu, id_varlon,'units', ncoutwv, id_lon)
  istatus=NF90_COPY_ATT(ncidu, id_varlon,'valid_min', ncoutwv, id_lon)
  istatus=NF90_COPY_ATT(ncidu, id_varlon,'valid_max', ncoutwv, id_lon)

  istatus=NF90_COPY_ATT(ncidu, id_varlat,'units', ncoutwv, id_lat)
  istatus=NF90_COPY_ATT(ncidu, id_varlat,'valid_min', ncoutwv, id_lat)
  istatus=NF90_COPY_ATT(ncidu, id_varlat,'valid_max', ncoutwv, id_lat)

  istatus=NF90_ENDDEF(ncoutwv)
  ! copy lon lat and time in the outwv file
  istatus=NF90_PUT_VAR(ncoutwv,id_lon,rlon)
  istatus=NF90_PUT_VAR(ncoutwv,id_lat,rlat)
  istatus=NF90_PUT_VAR(ncoutwv,id_time,rtime)

  istatus=NF90_CLOSE(ncidu)

  istatus= NF90_OPEN(cfileu,NF90_NOWRITE,ncidu)
  istatus= NF90_OPEN(cfilev,NF90_NOWRITE,ncidv)
  istatus=NF90_INQ_VARID(ncidu,cvaru,id_varu)
  istatus=NF90_INQ_VARID(ncidv,cvarv,id_varv)
  ! RD : not working with scale factors
  !istatus=NF90_GET_ATT(ncidu,id_varu,'missing_value',spval)

  ierr1 = NF90_GET_ATT(ncidu, id_varu, 'scale_factor', rsfu)
  ierr2 = NF90_GET_ATT(ncidu, id_varu, 'add_offset',   raou)

  IF ( (ierr1 /= NF90_NOERR).OR.(ierr2 /= NF90_NOERR) ) THEN
       rsfu = 1.; raou = 0.
  ENDIF

  ierr1 = NF90_GET_ATT(ncidv, id_varv, 'scale_factor', rsfv)
  ierr2 = NF90_GET_ATT(ncidv, id_varv, 'add_offset',   raov)

  IF ( (ierr1 /= NF90_NOERR).OR.(ierr2 /= NF90_NOERR) ) THEN
       rsfv = 1.; raov = 0.
  ENDIF


  DO jt=1,nt
      istatus=NF90_GET_VAR(ncidu,id_varu,varu(:,:),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
      istatus=NF90_GET_VAR(ncidv,id_varv,varv(:,:),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
      
      varu(:,:) = rsfu * varu(:,:) + raou
      varv(:,:) = rsfv * varv(:,:) + raov
      WHERE (varu /= spval) 
         varw(:,:)= SQRT( varu*varu +varv*varv)
         varwu(:,:)=varw(:,:)*varu(:,:)
         varwv(:,:)=varw(:,:)*varv(:,:)
      ELSEWHERE
         varw(:,:)=spval
         varwu(:,:)=spval
         varwv(:,:)=spval
      END WHERE
      istatus=NF90_PUT_VAR(ncoutwu,id_varwu,varwu(:,:),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
      istatus=NF90_PUT_VAR(ncoutwv,id_varwv,varwv(:,:),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
  END DO

     istatus=NF90_CLOSE(ncidu)
     istatus=NF90_CLOSE(ncidv)
     istatus=NF90_CLOSE(ncoutwu)
     istatus=NF90_CLOSE(ncoutwv)

   END PROGRAM mkmodxu
