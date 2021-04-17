include make.macro

EXE = GrIS_chk.x GrIS_chk_bat.x GrIS_chk_double.x GrIS_CreateNemo.x GrIS_CreateNemoBis.x

all:  $(EXE)

GrIS_chk.x: GrIS_chk.f90
	$(FC) GrIS_chk.f90 -o GrIS_chk.x $(FFLAGS)

GrIS_chk_bat.x: GrIS_chk_bat.f90
	$(FC) GrIS_chk_bat.f90 -o GrIS_chk_bat.x $(FFLAGS)

GrIS_chk_double.x: GrIS_chk_double.f90
	$(FC) GrIS_chk_double.f90 -o GrIS_chk_double.x $(FFLAGS)

GrIS_CreateNemo.x: GrIS_CreateNemo.f90
	$(FC) GrIS_CreateNemo.f90 -o GrIS_CreateNemo.x $(FFLAGS)

GrIS_CreateNemoBis.x: GrIS_CreateNemoBis.f90
	$(FC) GrIS_CreateNemoBis.f90 -o GrIS_CreateNemoBis.x $(FFLAGS)

clean:
	rm -f *~ 

cleanx: clean
	rm -f *.x

help:
	@-echo "      Makefile targets :"
	@-echo "make all    : compile all GrIS tools : $(EXE)"
	@-echo "make clean  : remove \*~ file"
	@-echo "make cleanx : remove \*~ file and \*.x files"
