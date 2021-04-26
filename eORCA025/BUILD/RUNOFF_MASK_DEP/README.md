# Auxiliary files for RNF and RNFISF
## 1. Context
The (liquid) freshwater fluxes are introduced in the configuration via two different NEMO modules: (i) sbcrnf (for surface or river runoff) and (ii) sbcisf (Ice shelf and 
marine glaciers melting).  Both modules require a depth file giving the depth range on which the freshwater flux is applied. For RNF, depth range always from the surface to a
typical depth. For ISF, the depth range corresponds to the bottom of the iceshelf edge and the deeper depth of the grounding line. Those depth are maintained constant throughout
a full simulation. They should be independant on the frequencies of the data and on the kind of data we are using (climatological or interannual). This is why we choose to put this information on a separate file, rnf_dep file.

On the other hand, coastal point where some kind of runoff is applied, must be defined in a mask file, used for several purposes. Actually, the main use with the present NEMO code
and configuration is to avoid SSS restoring a the points which receive freshwater. As for the rnf_dep file,  the runoff mask won't change in time and we will also use a separate
file, rnf_mask file.

  
## 2. rnf_dep file:
   This file combine runoff depths coming from ISBA and GrIS, and rnfisf_depht coming from Greenland and Antarctica (3 different sources). There are no overlap (at least we hope! ) between 
the data set. So we just combine the 3 sources with the tool. For river runoff, we choose to use a 10m depth.  This value is choosen pragmatically. It must not have a great impact.


| file name                                      |  region    |   dep min        |   dep max   |
| ---------------------------------------------- |----------- |   -------------- | ----------- |
| eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc     | World-A-G  |      0           |     10      |
| eORCA025.L75_y1950-2020_1m_greenland_isfbis.nc | Greenland  |    sozisfmin     | sozisfmax   |
| eORCA025_rnfisf_b0.2_c3.0_d1.0_v0.0.nc         | Antarctic  |    sozisfmin     | sozisfmax   |
| **result :** eORCA025_rnf_dep.nc               | World      |    sozisfmin     | sozisfmax   |

## 3. rnf_mask file:
  Production of this file is very similar to ref_dep: the same 3 sources of information are merged in a single file. The only difference is that in the case of the mask, only one
variable is concerned, instead of two. (For historical reason, the value of the mask is 0 everywhere except on runoff point where it is 0.5).

| file name                                      |  region    | variable name  |
| ---------------------------------------------- |----------- | -------------- |
| eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc     | World-A-G  | socoefr        |
| eORCA025.L75_y1950-2020_1m_greenland_rnfbis.nc | Greenland  | socoefr        |
| eORCA025_mskisf_b0.2_c3.0_d1.0_v0.0.nc         | Antarctic  | mask_isf_front | In this case, the values are to be transformed to 0.5
| **result :** eORCA025_rnf_mask.nc              | World      | socoefr        |

## 4. Program [create_rnf_dep_mask.f90](./create_rnf_dep_mask.f90)
This program was written in order to perform the merging between the various files reported above. Information about the files and variables name is hard coded.
It corresponds to the actual eORCA025 case.  A namelist can be provided to alter the default names. 

```fortran
&namcrdm
  ! RUNOFF depth files
  !-------------------
  cf_rnfGrIs_dep      = 'eORCA025.L75_y1950-2020_1m_greenland_isfbis.nc'
    cv_rnfGrIs_depmin = 'sozisfmin'
   cv_rnfGrIs_depmax  = 'sozisfmax'

  cf_rnfISBA_dep      = 'eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc'
    cv_rnfISBA_depmin = 'N/A'
    cv_rnfISBA_depmax = 'N/A'

  cf_rnfAnta_dep      = 'eORCA025_rnfisf_b0.2_c3.0_d1.0_v0.0.nc'
    cv_rnfAnta_depmin = 'sozisfmin'
    cv_rnfAnta_depmax = 'sozisfmax'

  ! Output file and variable
  cf_rnf_dep          = 'eORCA025.L75_rnf_dep.nc'
    cv_rnf_depmin     = 'sozisfmin'
    cv_rnf_depmax     = 'sozisfmax'

  ! RUNOFF mask files
  !------------------
  cf_rnfGrIs_msk      = 'eORCA025.L75_y1950-2020_1m_greenland_rnfbis.nc'
    cv_rnfGrIs_msk    = 'socoefr'

  cf_rnfISBA_msk      = 'eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc'
    cv_rnfISBA_msk    = 'socoefr'

  cf_rnfAnta_msk      = 'eORCA025_mskisf_b0.2_c3.0_d1.0_v0.0.nc'
    cv_rnfAnta_msk    = 'mask_isf_front'

  ! Output file and variables
  cf_rnf_msk          = 'eORCA025.L75_rnf_msk.nc'
    cv_rnf_msk        = 'socoefr'

  ! Grid information on file ( corresponding to cf_rnfGrIs_msk )
  cl_x   = 'x'          ! x- dimension name
  cl_y   = 'y'          ! y-dimension name
  cv_lon = 'nav_lon'    ! longitude (2D) variable name
  cv_lat = 'nav_lat'    ! latitude (2D) variable name
/

```

Note that for Antarctica, we use `mask_isf_front` to infer the runoff mask. We just test for a non zero value.


