
F90=ifort
FFLAGS= -O3 $(NCDF) -I$(NETCDF_INCDIR) -I$(NETCDFFORTRAN_INCDIR) $(NETCDF_LDFLAGS) $(NETCDFFORTRAN_LDFLAGS)
EXEC=mean_nemo.exe

.PHONY: all

all: $(EXEC)

mean_nemo.exe: mean_nemo.f90
	$(F90) mean_nemo.f90 -o mean_nemo.exe $(FFLAGS)


