FFLAGS = -O -CB -g -traceback -lnetcdf -lnetcdff

all: mkorca05 mkbathy05

mkorca05: mkorca05.f90
	ifort mkorca05.f90 -o mkorca05 $(FFLAGS)

mkbathy05: mkbathy05.f90
	ifort mkbathy05.f90 -o mkbathy05 $(FFLAGS)
