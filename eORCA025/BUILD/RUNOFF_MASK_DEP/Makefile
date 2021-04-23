include make.macro

all: create_rnf_dep_mask.x

create_rnf_dep_mask.x: create_rnf_dep_mask.f90
	$(FC) $(FFLAGS) create_rnf_dep_mask.f90 -o create_rnf_dep_mask.x

clean:
	@-\rm -f *~

