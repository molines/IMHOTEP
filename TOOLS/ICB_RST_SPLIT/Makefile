include make.macro

all: icbrstsplit.exe

icbrstsplit.exe : icbrstsplit.f90
	$(F90) $(FFLAGS) icbrstsplit.f90 -o icbrstsplit.exe


clean:
	rm -rf icbrstsplit.exe
