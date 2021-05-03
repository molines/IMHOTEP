# eORCA025 configuration setup
## 1. Numerical Code
 Decision is to be made on the NEMO release that will be used in the project. There is a trade-off between 
stability and novelty when choosing the NEMO release for the projetct.  
Two options are on the table:
  * NEMO r4.0.5 : this is  the last stable version of NEMO 4.0.
    * Advantage : stability, other run already performed with this release, well debugued.
    * Disadvantages : For the project, the concern is mainly about AGRIF development. 4.0.5 do not have the 
recent AGRIF developpement, that can me usefull for the second part of the project.
 * NEMO trunk : this is the sharp edge of NEMO on going developpent, that will lead to a the 4.2_RC (Release Candidate) 
in June or July 2021.
   * Advantage : this 4.2_RC, have various AGRIF enhancement, of interest for IMHOTEP:
     * AGRIF nests across the periodic boundary
     * AGRIF parallelism for various nests at the same level
     * On the side of advantages, the fact of using a pionnering new version and receive the aknowledgment of 
the communauty ! 
   * Disadvantages : unstable version till the 4.2_RC. And avalanche of bug fixes foreseen during the first 
few month of 4.2_RC.


**Decision : Use of NEMO at release 4.0.5** (update : actually minor release **4.0.6** )

## 2. Preparing Input files
Input data files are independant of the NEMO release to be used (at least I hope so.), and they can be prepared ASAP. 
Among these data files, there are domain configuration file, initial condition files, atmospheric forcing file (or at 
least the corresponing weight files), distance to the coast file for SSS restoring, local enhancements in eORCA025 
(bottom friction, lateral condition free-slip/no-slip), and of course the runoff files, that deserve a detailed work.

In order to able to rebuild all the input files, and for tracability, an effort is made to document all actions performed in the process of
producing the input files. For each kind of file there is a corresponding directory in [BUILD](./BUILD/) holding the scripts or dedicated program
realised for this specific file. In addition, each directory has its own README.md with detailled documentation on every step use for
the creation of the file.  This preparation document draws the road-map for the actions to perform in view of the building of input files. Clicking on the title of the following chapters, link you directly to the corresponding README file.

### 2.1 [Configuration file](./BUILD/DOMAIN_cfg/README.md)
This file defines the numerical grid (horizontal and vertical) including the corresponding metrics. In the creation
process we need a coordinate file (for the horizontal grid), a bathymetric file together with a set of streching coeficients
in order to define the vertical grid, with partial cells.  

We will take the coordinates and bathymetry files used by Pierre Mathiot when setting up his eORCA025.L121 configuration.
Vertical stretching parameters will be those of the standard DRAKKAR 75 levels, (as in OCCIPUT, for instance):
  * Bathymetry : eORCA025_bathymetry_b0.2.nc **update** : bathymetry was modified along Greenland coast and is finally eORCA025_bathymetry_b0.2_closed_seas_greenland.nc. 
  * Coordinates : eORCA025_coord_c3.0.nc 
  * DOMAIN_cfg tool : from DCM/4.0.5 (identical to DCM/4.0.6)
  * Namelists for make_domain_cfg.exe  are available in [this  directory](./BUILD/DOMAIN_cfg)
  * A description step by step of the build procedure is reported in the [README](./BUILD/DOMAIN_cfg/README.md) file 
placed in this directory.

### 2.2 [Initial conditions files, and restoring files (if any).](./BUILD/INITIAL_CONDITIONS/README.md)
There are quite a few available data set for T and S climatology. Among them, the World Ocean Atlases (ex-Levitus), with 
recent releases (last being 2018 release). Also of interest is the EN4 gridded data set, which is probably a bit biased 
toward recent years. There are also many products based on ARGO float remapping.  A choice must be done.  

**Decision : use of 1981-2010 climatology  for initial conditions.**


Also worth to be mentioned is the 3D T-S restoring to be used (or not ?) in some areas. In NEMO (with DRAKKAR enhancement),
initial conditions on T-S and the restoring data set can be different. Traditionnally, in DRAKKAR-like runs, we use to
have restoring on TS, in a very small patch downstream of Gibraltar Strait (in order to restore the Med sea water at
the right depht), but also on a much wider area in the southern Ocean, in the bottom layer corresponding to AABW, south of
30 South (which is defined by a sigma-2 surface from a climatology).  This latter restoring (in the southern ocean) maintains
the AABW which otherwise tend to disappear by vertical mixing and because they are not well alimented by the dense water
overflow off antarctica continental shelf. The consequence of AABW disparition is the spining down of the ACC, which  can be
as low as 95 Sv at Drake passage, after 50 years of run.  If the decision is to keep the AABW restoring we may think about the
data set to use, for this particular restoring. (In the past, we used the Gouretski annual climatology, which was computed
in a density framework. But this is now a rather old dataset... ). 

 > A new Gouretski climatology is now available (2018).  This new Gouretski climatology offer a monthly climatology for T,S.
 > Data set was retrieved from the [DKRZ web-site](https://cera-www.dkrz.de/WDCC/ui/cerasearch/entry?acronym=WAGHC_V1.0) (after
 > registration. The details of this climatology are given in this [paper](https://doi.org/10.1080/16742834.2019.1588066). 
 > Some reformating was performed (creating a single monthly file of temperature, salinity. Computing potential temperature).
 > This data set is now kept on ige-meom-cal1:/mnt/meom/DATA_SET/GOURETSKI_2018

**Decision : use Gouretski monthly climatology for restoring.**

> **UPDATE** : unstability problem starting the simulation when restoring toward Gouretski climatology, *to be insvestigated*.


We prepared initial conditions from WOA18. In fact I prepared 3 sets : first one from 1955-1964 decade climatology, available in WOA18,
another from 1981-2010 climatology, and a last one with 1955-2017 long term monthly climatology.  Details and comments regarding this preparation are described in a [technical note](./BUILD/WOA2018/WOA18_processing.md). 

The creation of the NEMO files is described in this other [document](./BUILD/INITIAL_CONDITIONS/README.md). From our experience, having good and clean initial
conditions, double checked is a key point for having a smooth simulation... This is the reason for which I prefer initial conditions on the model grid rather than on a regular grid with '3D interpolation on the fly' and weight files.

The same processing was done for Gouretski monthly climatology, with sosie. Scripts and namelists are available in [this directory](./BUILD/GOURETSKI_18)

### 2.3 [Distance-to-the coast file](./BUILD/DISTCOAST/README.md)
This file is used for the SSS restoring in order to prevent restoring in the vicinity of the coast, letting the  boundary currents
construct the runoff plumes at the scales allowed by the model resolution (and not present in the restoring data set). 

If this file is simply build from surface *tmask* for instance, some side effects occurs in areas where there are plenty of 
small islands, like in the western tropical pacific: If these islands are taken as land point, a large part of the ocean around
this constellation of islands does not have SSS restoring, which is really bad, because this area is where the correction is
efficient to counter balance poorly represented precipitations !  Hence, the surface *tmask* must be carefully edited, drowning
small islands, in order to have a distance-to-the-coast file that takes into account only main continental land. This is done using
**BMGTOOLS**, an interactive java-based program initially dedicated  to tunning the bathymetry. Detailed procedure is discribed in
this [document](./BUILD/DISTCOAST/README.md). Note that in the procedure we use a dummy very large value for the distance,
in some closed seas (Med Sea, Black Sea) in order to maintain SSS restoring even near the coast.

After this first file was build we came to preparing the runoff files, and using ISBA climatology there are several input of freshwater in the indonesian area, so that I think that
we musk keep the coast line with runoff in the computation of the distance to the coast file...

### 2.4 Atmospheric forcing files
Decision is to be made on the forcing data set. Candidates are : DFS5.2, JRA55, ERA5. 

**Decision: Use of JRA55, because (1) Widely used in OMIP, (2) grants forcing till present.**

#### 2.4.1 Preparing the forcing file
It is  important to note that atmospheric fields from reanalysis, of course, have a global coverage, for oceans and land. 
Prior to interpolation, reanalysis fields must be modified in such a way that land points are replaced by an extrapolation 
of ocean points (so called 'drowning' process). If this is not done, interpolated sea values may feel the influence of 
(very different) land values.  The 'drowning' process can be performed with the *mask_drown* program of the SOSIE serie.

#### 2.4.2 [Building weight files](./BUILD/WEIGHTS/README.md)
NEMO allows the use of atmospheric forcing on their native relatively low resolution grid. Therefore, a set of weight files
is needed for 'interpolation on the fly' (IOF). For wind components, we use a bicubic interpolation in order to ensure a 
continuous curl (first derivative). For other fields, a bilinear interpolation is used. 

Building weight files requires information on both source and target grid.  The detailed process of  building weights 
and preparing atmospheric fields is described in this [document](./BUILD/WEIGHTS/README.md).

Although not atmospheric forcing, we decided to use geothermal heating from the bottom of the ocean, using the Goutorbe data set,
proposed on a regular 1 degree grid. Weight files for bilinear interpolation were also built in the same way.

### 2.5 Auxiliary Enhancement files
#### 2.5.1 [Bottom friction](./BUILD/BFR2D/README.md)
For eORCA025, we use a locally enhanced bottom friction, in order to control the flow across some specific straits:
Torres Strait, Bering Strait, Gibraltar Strait. This is done through a kind of mask file that defines the areas
where enhancement is applied.

#### 2.5.2 [Lateral friction ( shlat2D)](./BUILD/SHLAT2D/README.md)
NEMO offers a wide range of lateral boundary conditions from free-slip to 'strong' slip. In NEMO, the lateral condition is 
controlled by the so-called *shlat* coefficient, which can be defined as a 2D field, read in an external file. We use
to have lateral friction modification in some specific straits, (with the same will to reduce the flow, just as we do with 
bottom friction), but also for full sub-areas such as the Mediterranean Sea, along the Antarctic Coast, and for some spots
in the Labrador Sea, along the West Greenland coast.  Playing with this condition is rather empirical, and results from 
many years of model tunning in the communauty. No strict rationale can be provided (intent of defining lateral rugosity
did not lead to convincing results... ). 

#### 2.5.3 [3D restoring coefficient file:](./BUILD/RESTO/README.md)
We decided to create the so called `resto.nc` file out of NEMO, in order to use the standard tradmp NEMO code. This will
ease the tracability of the configuration.
#### Overflow regions:
  * Gulf of Cadix, downstream Gibraltar strait, between 600 and 1300 m depth
  * Gulf of Aden, downstream Bab-el-Mandeb, output of the Red Sea
  * Arabian Gulf, downstream Ormuz strait, output of the Persian Gulf
##### Semi-enclosed seas
  * Black Sea
  * Red Sea
  * Persian Gulf
##### Deep waters in the Southern Ocean.
  * Restoring of the AABW in the southern ocean, defined by density sigma-2 > 37.16 (Rintoul classification)

### 2.6 Continental freshwater fluxes files
This is the central part of IMHOTEP, and a particular attention is brought to the building of input files used in NEMO for introducing these freshwater fluxes. 
For NEMO, the runoff file is a netcdf file on the model grid, with values of the time 
dependent freshwater flux (in kg/m2/s) given for *ad hoc* grid cells. It also contain a mask where *ad hoc* grid cells have
the value of 0.5 instead of 0. elsewhere.  The technical details of howto prepare the runoff files are given in [this document](./BUILD/RUNOFF_ISBA/README.md). 

#### 2.6.1 Liquid runoff:
After several comparision between available data set, decision was take to use the global ISBA reanalysis giving river discharge 
on 1/2 degree regular grid.  Comparision with observations, for the main rivers (Amazon, Orinoco, Mississipi, Congo, Niger, Gange, Bramhapoutre, Irriwady)
showed that the reanalysis well captures the interannual variability which indicated that is is suitable for the goal of the project.

Interannual ISBA runoff will be used everywhere in the global ocean except around Antarctica and Greenland. 
   * for antarctica we will use the iceshelf parametrization (ISF NEMO module) of Pierre Mathiot with climatological input files
   * for Greenland, Jeremie Mouginot prepare interannual file for the runoff (260 selected points).

#### 2.6.2 Solid runoff (icebergs calving).
Decision was taken to use calving flux and explicit representation of the icebergs (ICB NEMO module). This module is fed by annual calving rate (km3/year)
applied at specific locations around Antarctica and Greenland.  This information is read in NEMO through the `sn_icb` structure. Note that in the second part of the projet, when
AGRIF zoom will be deployed in the North Atlantic, we know that icebergs are not crossing the AGRIF boundaries.  We have analysed that most of the icebergs are drifting southward so
that probably very few may have the tendency to cross the Northern Agrif boundary.

For Antarctica we will use the input files prepared by Pierre Mathiot, using an annual climatology coherent for calving rate and basal iceshelf melting (for liquid contribution).

For Greenland, we will use interannual files prepared by Jeremie Mouginot. Calving around Greenland mainly occurs in fjords which are not well resolved with the eORCA025
configuration. Therefore some specific treatments are being discussed to convert part of the calving rate at the glacier front into liquid runoff at the entrance of the fjords.

We do note that on Pierre Mathiot file, there is also a contribution for the northern hemisphere, around Greenland, but also for very few spots in Svalblard and Canadian Archipelago.
These areas are not covered by Jeremie Mouginot work.  With regard to the AGRIF strategy  in the second part of the project, it is probably safer to just ignore this (small) contribution

Details of the construction of the calving files are given in this [technical note](./BUILD/CALVING/README.md).

#### 2.6.3 Spinup strategy.
IMHOTEP first run will use climatological (seasonal) input for the runoff (liquid and solid). 

For ISBA based runoff, the long term 1979-2018 daily climatology will be used.

For Greenland,  both liquid and solid contributions are pretty stable in the period 1959-1990 and then (from 1990 to present) show strong trends 
and variations. Therefore, we decided to use the 'stable' climatology (based on 1959-1990 period) for the spinup run.  Sensitivity run, with 
interannual runoff/calving variability will start in 1980, hence from a stabilized pre-90's state.

For Antarctica, we only have annual climatology that will be used through the whole project.

## 3. Preparing run time files: NEMO version dependent.
At run time, NEMO requires control input files such as namelists for the ocean and the sea-ice, as well as a
set of xml files describing the I/O strategy in term of data output for XIOS.
### 3.1 [Ocean namelist](eORCA025.L75-IMHOTEP00/CTL/namelist.eORCA025.L75-IMHOTEP00)
### 3.2 [Sea-ice namelist](eORCA025.L75-IMHOTEP00/CTL/namelist_ice.eORCA025.L75-IMHOTEP00)
### 3.3 XIOS control files (xml files)

## 4. Performance optimisation:
A scalability experiment was perfomed on jean-zay, from 240 cores (6 nodes) to 2400 cores (60 nodes). For this experiment, we choose to use 4 xios servers, running on a separated
computing node. Three-days runs (216 time steps of 1200 sec) were used, and  the last day was taken for evaluating the performance (measured as step/mn). On figure 1 we present the scalability diagram.

<img src=../Doc/Figures/scalability.png  width=130% />

On this busy picture, X-axis  corresponds to the number of cores used for NEMO.  Blue points, corresponds to the actual performance (step/mn, left Y-axis), and red points 
are the equivalent, assuming a perfect scalability (with reference to the 280 cores case). Brown points show the efficiency (%, left Y-axis), which is the ratio between actual and theoretical performances.  Except for some outliers, the efficiency is very close to (or above) 100%, until 1800 cores, but still very good up to 2400 cores, (85%). According to these
performances, yellow points indicates the elapsed time for 1yr of simulation (hours, right Y-axis), and green points the CPU hours for 1 year (hoursx10, left Y-axis). Due to the 
good scalability, the CPU hours are very stable (except for the outliers), around 2500 hours/years. In general, the performance decreases somehow when the model spins-up and when
the number of icebergs is stabilized. So, regarding this experiment, we can estimate  that 1year of eORCA025 experiment should not cost more than 3250 hours (taking a margin of 30%).

The sweet spot for this experiment is probably around 1500 cores, considering the trade off between queue waiting-time and elapsed time. 


