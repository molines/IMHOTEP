# Makefile for aqviso_fdbk

include make.macro

ALL: aviso_fdbk.exe fdbk_reader_alt.exe fdbk_reader_alt_tab.exe

aviso_fdbk.exe : aviso_fdbk.f90
	$(F90) $(FFLAGS) aviso_fdbk.f90 -o aviso_fdbk.exe

fdbk_reader_alt.exe : fdbk_reader_alt.f90
	$(F90) $(FFLAGS) fdbk_reader_alt.f90 -o fdbk_reader_alt.exe

fdbk_reader_alt_tab.exe : fdbk_reader_alt_tab.f90
	$(F90) $(FFLAGS) fdbk_reader_alt_tab.f90 -o fdbk_reader_alt_tab.exe

clean:
	\rm -f *~
	\rm -f *.exe
