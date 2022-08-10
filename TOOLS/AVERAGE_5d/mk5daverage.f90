PROGRAM mk5daverage
  !!======================================================================
  !!                     ***  PROGRAM  <module>  ***
  !!=====================================================================
  !!  ** Purpose :
  !!
  !!  ** Method  :
  !!
  !! History :  4.0  : 08/2022  : J.M. Molines : 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------


  !!----------------------------------------------------------------------
  !! IMHOTEP , MEOM 2022
  !! $Id$
  !! Copyright (c) 2022, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/CDFTOOLSCeCILL.txt)
  !!----------------------------------------------------------------------
  USE netcdf
  IMPLICIt NONE
  INTEGER(KIND=4) :: ji,jj,jk,jt
  INTEGER(KIND=4) :: narg, iargc, ijarg
  INTEGER(KIND=4) :: ifil
 
  
  CHARACTER(LEN=80), DIMENSION(6) :: cf_in
  CHARACTER(LEN=80), DIMENSION(6) :: cv_in
  CHARACTER(LEN=80) :: cf_out
  CHARACTER(LEN=80) :: cv_out
  CHARACTER(LEN=80) :: cv_e3
  CHARACTER(LEN=80) :: cl_dum

  LOGICAL           :: lnc4 = .false.
  LOGICAL           :: lvvl = .false.
  !!---------------------------------------------------------------------- 
  narg=iargc()

  IF ( narg == 0 ) THEN
     PRINT *,' usage :  mk5daverage -l FILES_1-5 -lvar VAR-lst -nc4 -o OUT-file -vvl E3-file'
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       '
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'       '
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'       '
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       '
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file : ', TRIM(cf_out) 
     PRINT *,'         variables : ', TRIM(cv_out),' (    )'
     PRINT *,'      '
     PRINT *,'     SEE ALSO :'
     PRINT *,'      '
     PRINT *,'      '
     STOP
  ENDIF

  ijarg = 1 
  DO WHILE ( ijarg <= narg )
     CALL getarg(ijarg, cldum ) ; ijarg=ijarg+1
     SELECT CASE ( cldum )
     CASE ( '-l'   ) ; ifil=1 ; CALL getarg(ijarg, cf_in(ifil) ) ; ifil=ifil+1 ; ijarg=ijarg+1
                              ; CALL getarg(ijarg, cf_in(ifil) ) ; ifil=ifil+1 ; ijarg=ijarg+1
                              ; CALL getarg(ijarg, cf_in(ifil) ) ; ifil=ifil+1 ; ijarg=ijarg+1
                              ; CALL getarg(ijarg, cf_in(ifil) ) ; ifil=ifil+1 ; ijarg=ijarg+1
                              ; CALL getarg(ijarg, cf_in(ifil) ) ; ifil=ifil+1 ; ijarg=ijarg+1
                              ; CALL getarg(ijarg, cf_in(ifil) ) ; ifil=ifil+1 ; ijarg=ijarg+1
     CASE ( '-lvar' )         ; CALL getarg(ijarg, cv_in
        ! option
     CASE ( '-o'   )          ; CALL getarg(ijarg, cf_out     ) ; ijarg=ijarg+1
     CASE ( '-nc4' )          ; lnc4 = .TRUE.
     CASE ( '-vvl' )          ; lvvl = .TRUE.    ; CALL getarg(ijarg, cv_e3 ) ; ijarg=ijarg+1
     CASE DEFAULT             ; PRINT *, ' ERROR : ', TRIM(cldum),' : unknown option.'; STOP 1
     END SELECT
  ENDDO

END PROGRAM mk5daverage
