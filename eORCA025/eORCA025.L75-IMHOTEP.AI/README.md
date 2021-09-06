# IMHOTEP.AI 
## Description
This experiment uses the exact same code than IMHOTEP.S. The same correction term is added to the precip and no SSS restoring is used.

## Continental Fresh Water Fluxes:
All fluxes are interannual except for Greenland where they are climatological. The difference is visible in the namelist:

In namsbc_rnf_drk, Greenland runoff file is eORCA025.L75_1m_greenland_rnfbis_clim_y1950-1972.nc, corresponding to the 1950-1972 climatology.  
In namsbc_isf_drk, Greenland isf_runoff file is eORCA025.L75_1m_greenland_isfbis_clim_y1950-1972.nc, corresponding to the 1950-1972 climatology.  
In namberg_drk, Greenland calving file is eORCA025.L75_1m_greenland_calvingbis_clim_y1950-1972.nc, corresponding to the 1950-1972 climatology.  



