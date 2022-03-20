# Makefile for FORCING_TOOLS in IMHOTEP
# ( make.macro is a link that points to the file macro.xxx where 
#   xxx is representative of your machine )
include make.macro

BINDIR = ../bin
VPATH = $(BINDIR)

EXEC = mkclimato.exe mkw10.exe mkmodxu.exe

.PHONY: all 

all: $(EXEC)


mkclimato.exe: mkclimato.f90
	$(F90) $(FFLAGS) mkclimato.f90 -o $(BINDIR)/mkclimato.exe $(NCDF)

mkw10.exe: mkw10.f90
	$(F90) $(FFLAGS) mkw10.f90 -o $(BINDIR)/mkw10.exe $(NCDF)

mkmodxu.exe: mkmodxu.f90
	$(F90) $(FFLAGS) mkmodxu.f90 -o $(BINDIR)/mkmodxu.exe $(NCDF)

clean:
	@\rm *~

cleanexe: clean
	( cd $(BINDIR) ; \rm -f $(EXEC)

