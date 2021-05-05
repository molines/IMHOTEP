# Runoff like file for ISF parameterization
## 1. Context:
We choose to represent the freswater flux coming from iceshelf basal melting as a runoff applied at depth, between a minimum depth 
corresponding to the ice draft at the iceshelf edge, and a maximum value corresponding to the depth of the grounding line (where 
the continental icesheet, starts to float on the seawater). The parameterization also takes into consideration the latent heat 
consumed by the process of ice melting, which make it different from simple runoff.  

The freshwater flux for a given isceshelf is taken from various estimates: For Antarctica, annual climatological estimates from 
Rignot et Al (2013) are used.  For Greenland, we use monthly interannual estimates produced by Mouginot et al (2021). However, 
for Greenland, the freshwater melting estimates correspond to the very few iceshelves but mainly to marine glaciers that calve 
icebergs from their front. For the parameterization we consider the freswater flux (liquid) produced by the melting of the 
icebergs in the fjords, before reaching the open ocean, or the first ocean point in the model.  This means that we need an estimate 
of the proportion of the icebergs that melt in the fjord. A review of the sparse litterature on this topics gives some estimates 
between 30% and 80%. We thus take a rough estimates of 50%. 

## 2. Building rnf_isf file for Antarctica
The starting point for all actions related to antarctic iceshelves is the icedraft. This field is part of the bathymetric file 
and gives the topography of the base of the iceshelf.  The cavity formed between the ocean bottom and the base of the iceshelf 
has a thickness of : bathymetry - icedraft.  Data are given for each iceshelf and the tricky part is to connect some areas of 
the icedraft to iceshelf name. Program `cdfisf_fill` developped by Pierre Mathiot in the frame of CDFTOOLS produce a file mask 
where each iceshelf is identified by a unique ID.  For our actual configuration, we use the already prepared mask file:
`eORCA025_mskisf_b0.2_c3.0_d1.0_v0.0.nc`. On the other hand date are collected in an ASCII file 
[eORCA025_mskisf_c3.0_v0.0.txt](./eORCA025_mskisf_c3.0_v0.0.txt) giving the following information for all retained iceshelves:

| ID  | name | lon  | lat | iseed | jseed |  hmin  | hmax |  melting | calving | flag   |
| --- | ---- | ---- | --- | ----- | ----- | ------ | ---- | -------- | ------- | ------ |
| ... | ...  | ...  | ... | ..... | ..... | ...... | .... | ........ | ....... | ...... |
| 18  | ROSS |  0.0 | 0.0 | 440   |  110  |  200   |  950 |  47.7    | 146.3   | .FALSE.|
| ... | ...  | ...  | ... | ..... | ..... | ...... | .... | ........ | ....... | ...... |

Melting and calving are given in GT/year. As a matter of fact, for building the runoff file we only use ID, hmin, hmax and melting.

Program `cdfisf_rnf` developped in the frame of the CDFTOOLS produce the  rnf_isf file with the following variables, spreading 
the melting over the iceshelves edges.
  * sozisfmax : depth in meter of the maximum depth for runoff
  * sozisfmin : depth in meter of the minimum depth for runoff
  * sofwfisf  : climatological annual liquid discharge in kg/m2/s. 

For consistency with the configuration this file is named `eORCA025_rnfisf_b0.2_c3.0_d1.0_v0.0.nc` (sorry for that :). A bash 
script ([mkrnfisf.sh](./mkrnfisf.sh)) is used as a wrapper of the cdfisf_rnf tool.


## 3. Building rnf_isf file for Greenland
For Greenland, we start from the GrIS file `eORCA025_GrIS_forcing_50percent_solid.nc`  produced by Mouginot et Al, giving the 
liquid and solid freshwater fluxes and the associated depth, corresponding to 262 sources around Greenland. The file already 
gives the (I,J) coordinates of the coastal model point (eORCA025). This file is available on ige-meom-cal1.u-ga.fr:/mnt/meom/DATA_SET/RUNOFF_GrIS/. 
A detailed description of issues raised when using this data set (in particular concerning the coastal bathymetry of the model, 
is given in [this document](../RUNOFF_GREENLAND/README.md). As far as only rnf_isf is concerned here, we should say that the 
rnf_isf file for Greenland was produced together with the rnf and calving file, using the dedicated program 
[GrIS_CreateNemoBis](../RUNOFF_GREENLAND/GrIS_CreateNemoBis.f90). After long discussion, we decided to put in this file, only the
contribution of the icebergs that melt into the fjords, fixed as 50% of the total calved icebergs.  This is because, associated to 
this freshwater flux, there is a latent heat flux that extract heat from the sea water when ice is melting. In the GrIS file, 
liquid contribution comes from coastal rivers and deep injection (below) the glacier) of melted and percolated waters, where 
the melting process occurs with heat exchange with the atmosphere, and not the ocean.  The final rnf_isf file for Greenland 
has the following variables:
  * sozisfmax : depth in meter of the maximum depth for rnf_isf. This depth possibly takes into account the depth of fjords sill that prevent deep waters in the fjords to connect to the open ocean.
  * sozidfmin : depth in meter of the minimum depth for rnf_isf. As this depth is also used for coastal runoff, it is set to 0m (surface). 
  * sornfisf :  monthly interannual liquid discharge (kg/m2/s) due to ice melting in the ocean.

The final file in this process hold the 71 years on monthly data: `eORCA025.L75_y1950-2020_1m_greenland_isfbis.nc`. For use in 
NEMO, it must be splitted into yearly files. This is done with the script [GrIS_Annual_Split.sh](../RUNOFF_GREENLAND/GrIS_Annual_Split.sh), 
producing files such as `eORCA025.L75_1m_greenland_isfbis_y1997.nc` for instance.

From this set of annual file, a long term monthly climatology (1950-1972) was computed, to be used in the climatological run 
([mk_clim_50-72.sh](../RUNOFF_GREENLAND/mk_clim_50-72.sh)). Note that we decided to compute the climatology over years where the 
Greenland icesheet was almost in equilibrium. For experiment using the interannual dataset, a trend in the freshwater fluxes will show up.

Depths related variable are  reprocessed to form a unique (Greenland, Antarctic) depth dataset. (see [RUNOFF_MASK_DEP](../RUNOFF_MASK_DEP/README.md) for details).

## 4. Nemo adaptation to deal with various input data for rnf_isf
This point is described in details in [this document](../../../Doc/Multiple_frequency_runoff.md).
