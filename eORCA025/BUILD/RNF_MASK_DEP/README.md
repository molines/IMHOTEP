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
the data set. So we just combine the 3 sources with the tool.


| file name                                      | dep min        |   dep max   |
| ---------------------------------------------- | -------------- | ----------- |
| eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc     |    0           |     10      |
| eORCA025.L75_y1950-2020_1m_greenland_rnfbis.nc |   hmin         |  hmax       |
|    Antarctic ?                                 |     N/A        |             |
| result :                                       |                |             |

## 3. rnf_mask file:
  Production of this file is very similar to ref_dep: the same 3 sources information are merged in a single file. The only difference is that in the case of the mask, only one
variable is concerned, instead of two.

| file name                                      | variable name  |
| ---------------------------------------------- | -------------- |
| eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc     | socoefr        |
| eORCA025.L75_y1950-2020_1m_greenland_rnfbis.nc | socoefr        |
|    Antarctic ?                                 |     N/A        |
| result :                                       |                |

