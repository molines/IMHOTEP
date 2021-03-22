# Building initial conditions for eORCA05.L75
As for eORCA025.L75 we used the WOA18 1980-2010 climatology (AKA decab81B0). 
This is done with `SOSIE` using the same namelists just changing the name of the `mesh_mask.nc` file.  
After Sosie processing, files were reformatted to NEMO standard with mknemolike.sh script.

## Extracting SSS file for sea surface salinity restoring:
For some nco limitation in ncwa with netcdf4/HDF5 files we use the following procedure to obtain the SSS file from `eORCA05.L75_81B0_WOA18_1m_vosaline.nc`

```
   ncks -d deptht,1 eORCA05.L75_81B0_WOA18_1m_vosaline.nc eORCA05.L75_81B0_WOA18_1m_SSS.nc
   # transform SSS file (with deptht dimension) into netcdf classic format:
   ncks -3 eORCA05.L75_81B0_WOA18_1m_SSS.nc eORCA05.L75_81B0_WOA18_1m_SSS.nc3
   # eliminate deptht dimension
   ncwa -a deptht eORCA05.L75_81B0_WOA18_1m_SSS.nc3 eORCA05.L75_81B0_WOA18_1m_SSS.nc1
   # transform SSS file with no deptht into netcdf4/HDF5 with deflation factor of 1
   ncks -4 -L1 eORCA05.L75_81B0_WOA18_1m_SSS.nc1 eORCA05.L75_81B0_WOA18_1m_SSS.nc4
   # Do some cleaning of intermediate files
   rm eORCA05.L75_81B0_WOA18_1m_SSS.nc3 eORCA05.L75_81B0_WOA18_1m_SSS.nc1
   # rename final file to its official name :
   mv eORCA05.L75_81B0_WOA18_1m_SSS.nc4 eORCA05.L75_81B0_WOA18_1m_SSS.nc
```

> In NEMO, 2D files must not have a degenerated vertical dimension. The latter procedure is the easiest way to eliminate such a dimension. It turned to be a bit more complicated tan it should be, just because of the NCO limitations.
