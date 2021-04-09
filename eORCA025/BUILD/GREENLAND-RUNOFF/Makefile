include make.macro

all:  GrIS_chk.x GrIS_chk_bat.x GrIS_chk_double.x

GrIS_chk.x: GrIS_chk.f90
	$(FC) GrIS_chk.f90 -o GrIS_chk.x $(FFLAGS)

GrIS_chk_bat.x: GrIS_chk_bat.f90
	$(FC) GrIS_chk_bat.f90 -o GrIS_chk_bat.x $(FFLAGS)

GrIS_chk_double.x: GrIS_chk_double.f90
	$(FC) GrIS_chk_double.f90 -o GrIS_chk_double.x $(FFLAGS)
