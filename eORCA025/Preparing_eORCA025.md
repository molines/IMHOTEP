# eORCA025 configuration setup
## 1. Numerical Code
The choice of NEMO version to use in the project is based on the following with 2 options on the table:
  * NEMO r4.0.6 : this is  the last stable version of NEMO 4.0.
    * Advantage : stability, other run already performed with this release, well debugued.
    * Disadvantages : For the project, the concern is mainly about AGRIF development. 4.0.6 do not have the 
recent AGRIF developpement, that can me usefull for the second part of the project.
  * NEMO trunk : this is the sharp edge of NEMO on going developpent, that will lead to a the 4.2_RC (Release Candidate) 
in June or July 2021.
    * Advantage : this 4.2_RC, have various AGRIF enhancement, of interest for IMHOTEP:
      * AGRIF nests across the periodic boundary
      * AGRIF parallelism for various nests at the same level
      * On the side of advantages, the fact of using a pionnering new version and receive the aknowledgment of 
the community ! 
    * Disadvantages : unstable version till the 4.2_RC. And avalanche of bug fixes foreseen during the first 
few month of 4.2_RC.

**Decision : Use of NEMO at release 4.0.6**

## 2. Preparing Input files
A big part of setting up a configuration consists in creating the input data files for NEMO.
Among these data files, there are domain configuration files, T-S initial conditions files, SSS files for restoring, atmospheric forcing file (or at 
least the corresponding weight files), distance to the coast file for SSS restoring, local enhancements in eORCA025 
(bottom friction, lateral condition free-slip/no-slip, 3D restoring coefficient file), and of course the freshwater fluxes files, that deserve a detailed work.

In order to be able to rebuild all the input files, and for tracability, an effort is made to document all actions carried out for
producing the input files. For each kind of file there is a corresponding directory in [BUILD](./BUILD/) holding the scripts or dedicated programs
realised for this specific file. In addition, each directory has its own README.md with detailled documentation on every step used for
the creation of the file.  This preparation document draws the road-map for the actions to perform in view of building input files. Clicking 
on the title of the following paragraphs, link you directly to the corresponding README file.

### 2.1 [Domain configuration file](./BUILD/DOMAIN_cfg/README.md)
This file defines the numerical grid (horizontal and vertical) including the corresponding metrics. In the creation
process we need a coordinate file (for the horizontal grid), a bathymetric file and a set of streching coeficients
defining the vertical grid, with partial cells.  

Coordinates and bathymetry files used by Pierre Mathiot when setting up his eORCA025.L121 configuration, are taken as a starting point.
Vertical stretching parameters will be those of the standard DRAKKAR 75 levels, (as in OCCIPUT, for instance):
  * Bathymetry : eORCA025_bathymetry_b0.2.nc **update** : bathymetry was modified along Greenland coast and is finally eORCA025_bathymetry_b0.2_closed_seas_greenland.nc. 
  * Coordinates : eORCA025_coord_c3.0.nc 
  * DOMAIN_cfg tool :  DCM/4.0.6  provides `make_domain_cfg.exe`
  * [Namelists](./BUILD/DOMAIN_cfg/namelist_cfg) for `make_domain_cfg.exe`  are available in [this  directory](./BUILD/DOMAIN_cfg)

### 2.2 [Initial conditions files, and restoring files (if any).](./BUILD/INITIAL_CONDITIONS/README.md)
There are quite a few available data set for T and S climatology. Among them, the World Ocean Atlases (ex-Levitus), with 
recent releases (last being 2018 release). Also of interest is the EN4 gridded data set, which is probably a bit biased 
toward recent years. There are also many products based on ARGO float remapping.

**Decision : use of WOA2018 1981-2010 climatology  for initial conditions.**

Also worth to be mentioned is the 3D T-S restoring to be used in some areas. In NEMO (with DRAKKAR enhancement),
initial conditions on T-S and the restoring data set can be different. Traditionnally, in DRAKKAR-like runs, we use to
have restoring on TS, in a very small patch downstream of Gibraltar Strait (in order to restore the Med sea water at
the right depht), but also on a much wider area in the Southern Ocean, in the bottom layer corresponding to AABW, south of
30 South (which is defined by a sigma-2 surface from a climatology).  This latter restoring  maintains
the AABW which otherwise tends to disappear by vertical mixing and because they are not well alimented by the dense water
overflow off antarctica continental shelf. The consequence of AABW disparition is the decrease of the ACC transport, which  can be
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

Initial conditions were thus prepared from WOA18 dataset. This data set proposes several monthly climatologies computed for several 
decades (from 1955-1964 to 1995-2004), a monthly climatology corresponding to the ARGO years (2005-2017) a monthly climatology 
computed over 30 years (1981-2010) and a long term monthly climatology (1955-2017). WOA18 is a gridded data set with 2 available
resolutions: 1 degree and 1/4 degree. Surprisingly, the 1-degree resolution gives better interpolated fields on the eORCA025 grid
than the 1/4-degree, in particular at high latitude (North and South). Therefore, the 1-degree data set was used throughout the
preparation of the initial conditions.   

In order to evaluate the differences, 3 set of data were prepared, corresponding to the 1955-1964 climatology, the 1981-2010 
climatology and the 1955-2017. Comparison between those different periods are shown on the DRAKKAR 
[monitoring site](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/A_WOA2018_CLIMATO/), in a very raw way (work under progress ...).

Decision was taken to use the 30-years climatology (1981-2010) for building the initial conditions, with the idea that the period of
sensitivity experiments in IMOTHEP starts in 1980. The period 1958-1979 is a spin-up period.

Details and comments regarding this preparation are described in a [technical note](./BUILD/WOA2018/WOA18_processing.md) while  
the creation of the NEMO files is described in this other [document](./BUILD/INITIAL_CONDITIONS/README.md). 

From our experience, having good, clean and double checked initial conditions, is a key point for having a smooth simulation. 
This is the reason why initial conditions are projected (and checked) on the model grid instead of keeping the original dataset
with '3D interpolation on the fly', providing weight files.

The same processing was done for Gouretski monthly climatology, with sosie. Scripts and namelists are available 
in [this directory](./BUILD/GOURETSKI_18).

### 2.3 [Distance-to-the coast file](./BUILD/DISTCOAST/README.md)
This file is used for the SSS restoring in order to prevent restoring in the vicinity of the coast, letting the  boundary currents
construct the runoff plumes at the scales allowed by the model resolution (and not present in the restoring data set). 

If this file is simply build from surface *tmask* for instance, some side effects occurs in areas where there are plenty of 
small islands, like in the western tropical pacific: If these islands are taken as land point, a large part of the ocean around
this constellation of islands does not have SSS restoring, which is really bad, because this area is where the correction is
efficient to counter-balance poorly represented precipitations! Hence, the surface *tmask* must be carefully edited, drowning
small islands, in order to have a distance-to-the-coast file that takes into account only main continental land. This is done using
**BMGTOOLS**, an interactive java-based program initially dedicated  to tunning the bathymetry. Detailed procedure is described in
this [document](./BUILD/DISTCOAST/README.md). Note that in the procedure we use a dummy very large value for the distance,
in some closed seas (Med Sea, Black Sea) in order to maintain SSS restoring even near the coast.

After this first file was build we came to preparing the runoff files, and using ISBA climatology there are several input of 
freshwater in the Indonesian area, so that another distance-to-the-coast file was build, defining the coast line with the model
points where freshwater discharge is applied.

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
is needed for 'interpolation on the fly' (IOF). For wind components, a bicubic interpolation is used, in order to ensure a 
continuous curl (first derivative). For other fields, a bilinear interpolation is used. 

Building weight files requires information on both source and target grid.  The detailed process of  building weights 
and preparing atmospheric fields is described in this [document](./BUILD/WEIGHTS/README.md).

Although not atmospheric forcing, we decided to use geothermal heating from the bottom of the ocean, using the Goutorbe data set,
proposed on a regular 1 degree grid. Weight files for bilinear interpolation were also built in the same way.

### 2.5 Auxiliary Enhancement files
#### 2.5.1 [Bottom friction](./BUILD/BFR2D/README.md)
For eORCA025, locally enhanced bottom friction is used, in order to control the flow across some specific straits:
Torres Strait, Bering Strait, Gibraltar Strait. A mask file (1/0) is needed to  define the areas where enhancement is applied.
The details of how to prepare this file is given in the corresponding [README](./BUILD/BFR2D/README.md) file.

#### 2.5.2 [Lateral friction ( shlat2D)](./BUILD/SHLAT2D/README.md)
NEMO offers a wide range of lateral boundary conditions from free-slip to 'strong' slip. In NEMO, the lateral condition is 
controlled by the so-called *shlat* coefficient, which can be defined as a 2D field, read in an external file.  Lateral 
friction modification in some specific straits, is classically used in our simulations, also in order to reduce the 
flow, just as we do with bottom friction, but also for full sub-areas such as the Mediterranean Sea 
and for some spots in the Labrador Sea, along the West Greenland coast.  Playing with this condition is rather 
empirical, and results from many years of model tuning in the community. No strict rationale can be provided (intent of 
defining lateral rugosity did not lead to convincing results...). 

#### 2.5.3 [3D restoring coefficient file:](./BUILD/RESTO/README.md)
Standard NEMO code use a so called `resto.nc` holding a 3D relaxation coefficient (sec^-1). Restoring is done where this
coefficient is non-zero. For the sake of tracability, `resto.nc` is now fully built during the preparation of the configuration.
(In DRAKKAR, is was partially built 'on the fly', for flexibility but it turns out that it makes the control difficult.)

Regions where 3D restoring is used are:
##### Overflow regions:
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
This is the central part of IMHOTEP, and a particular attention is paid to the building of input files used in NEMO 
for introducing these freshwater fluxes. 
#### 2.6.1 NEMO modules:
Basically, NEMO offer three different modules to take the continental freshwater flux into account: (i) the river runoff
module (RNF), (ii) the ice-shelf module (ISF) and (iii) the iceberg module (ICB).
  * In **RNF**, the river discharges (kg/m2/s) are introduced in coastal model cells, in the vicinity of the river mouths. Among
the many different techniques available in NEMO, decision was taken to introduce the corresponding freshwater flux over
a pre-defined depth (ie on many model cells on the vertical). 
  * In **ISF**, the iceshelf basal melting (given in kg/m2/s) is introduced just as in RNF, but on a much deeper depth range
(between the iceshelf draft at its edge and the depth of the grounding line). In addition, in ISF, the fresh water
flux in associated with a latent heat flux, applied at the model cells impacted by the freswater flux, and producing
some cooling of these cells.
  * In **ICB**, explicit representation of icebergs is carried out. The module is fed by a calving rate (GT/year) given for model
points at the edge of the iceshelves. At every time step, a certain amount of ice (according to the calving rate) is 
accumulated over 5 icebergs categories (small to big). For each category, a threshold is defined. When the threshold is
reached, one or several (depending on the category) icebergs  calve, and are treated as lagragian floats afterward.
They travel in the ocean under the influence of ocean currents and atmospheric winds. As soon as they calve, icebergs
start to melt and the resulting freshwater flux is applied at the cells occupied by the icebergs at a given instant. A latent
heat flux is also applied. Decision was taken to use this approach to deal with solid freshwater flux, at least for the runs
performed in the first phase of the project (ICB is not compatible with AGRIF nests).
  * In addition to the freshwater discharge files (liquid of solid) associated to the 3 NEMO modules, depth information for RNF and
ISF is also required, in two separated files.
#### 2.6.2 Data set:
Several data base are being used for preparing the fresh-water discharge files, and they have different frequencies. Some developpments 
were done in NEMO in order to deal with multi-dataset. (Details can be found in this [technical note](../Doc/Multiple_frequency_runoff.md)). 
This allow to have as many NEMO input files as data set, taking care of not having overlap between different files. Data set are:
  * **ISBA** land surface system provides **daily** (1979-2018) river discharge for the global ocean. Specific analysis were carried out for the 
biggest rivers, whose impact are being studied in IMHOTEP : Amazon, Orinoco, Niger, Congo, Brahmaputra, Ganges, Irriwady. ISBA
data are in general very consistent with observations, and the interannual variability is well captured. Decision was taken to use this data
set for all the rivers, except for Greenland and Antarctica. They will be used in **RNF** module. F.Papa, W. Llowel provide the data. 
(See the [preparation](./BUILD/RUNOFF_ISBA/README.md) of the corresponding file).
  * **GrIS** (Greenland Ice Sheet mass balance) data set is used for all freshwater discharges (solid and liquid) around Greenland.
It provides **monthly** discharges (1950-2018) for 262 points around the island. Unfortunatly, eORCA025 model grid is too coarse to
take into account the narrow fjords where most of the Greenland glaciers, and the surface runoff reach the sea.  
However, data provided by J. Mouginot, P. Mathiot and N. Jourdain consists in both solid and liquid freshwater discharges, at 
the coastal model grid points corresponding to the 262 original points. For each source a depth is associated, taking into 
account both the depth of glacier tongues and the depth of bathymetric sills in the fjords. Comparison of these  depths with the 
first-guess model depths clearly points out big discrepancies.  Hence, a new bathymetry for the model was derived from the 
BedMachineGreenland, high resolution bathymetric data, around Greenland and merged with the general eORCA025 bathymetry. 
See [details](./BUILD/GREENLAND-BATHY/README.md).  
For the solid contribution, it is known that a certain amount of the calved icebergs from the glacier front, just melt in the 
fjord before reaching the open ocean.  There are some estimate (quite few indeed) published in the litterature, giving the 
proportion of icebergs reaching the oceans over the total amount of calved icebergs at the glacier front.  This proportion 
is within a range of 30% to 80%.  In order to go ahead, a decision was taken to make a first  data set, assuming that 50% of 
the calved icebergs melt in the fjords, thus converting the equivalent solid discharge to liquid discharge.  
In summary : GrIS provides (1) FW fluxes for **RNF** (surface freshwater reaching the sea by coastal rivers or percolating through the glaciers),
(2) FW fluxes for **ISF** (50% of the iceberg mass melted in the fjords) and (3) calving rate for **ICB** (50% of the icebergs not melted in the fjords).  
See related details for [RNF and ICB](./BUILD/RUNOFF_GREENLAND/README.md) and for [ISF](./BUILD/RUNOFF_ISF_AA_GR/README.md) Greenland contributions.
  * **Rignot et al (2013)**  **annual climatological** data are used for Antarctica fresh water fluxes. Only iceshelf basal melting
and iceshelf calving rate are used (**ISF** and **ICB**). For eORCA025, data files were prepared by P. Mathiot, using specific CDFTOOLS that he 
developped for this purpose. (See Technical details for [ICB](./BUILD/CALVING/README.md) and [ISF](./BUILD/RUNOFF_ISF_AA_GR/README.md).  
    * It is worth to be noted that Pierre's file have some calving points in the Northern hemisphere. We just skip all these point, as far as
GrIS data set is used. Doing so, we skip very small calving contribution from Svalblard and Canadian Archipelago. In fact, in view of the second part
of the project, where AGRIF nest will be used in this are we want to avoid icebergs drifting toward the Arctic.
  * Depth files for RNF and ISF: Although there are mutiple data set with different frequencies, depths where freshwater fluxes are applied are centralized in 2 files,
one for each parameterisation. The files are created by merging the information of the data set with the program [create_rnf_dep_mask.f90](./BUILD/RUNOFF_MASK_DEP/create_rnf_dep_mask.f90). This program also creates a runoff mask where RNF or ISF model points are identified. (See details in this [document](./BUILD/RUNOFF_MASK_DEP/README.md).)

#### 2.6.3 Spinup strategy.
IMHOTEP first run will use climatological (seasonal) input for the runoff (liquid and solid). 
  * For ISBA based runoff, the long term 1979-2018 daily climatology will be used.
  * For Greenland,  both liquid and solid contributions are pretty stable in the period 1959-1990 and then (from 1990 to present) show strong trends 
and variations. Therefore, we decided to use the 'stable' climatology (based on 1959-1990 period) for the spinup run.  Sensitivity run, with 
interannual runoff/calving variability will start in 1980, hence from a stabilized pre-90's state.
  * For Antarctica, we only have annual climatology that will be used throughout the whole project.

## 3. Preparing run time files: NEMO version dependent.
At run time, NEMO requires control input files such as namelists for the ocean and the sea-ice, as well as a
set of xml files describing the I/O strategy in term of data output for XIOS.
### 3.1 [Ocean namelist](eORCA025.L75-IMHOTEP00/CTL/namelist.eORCA025.L75-IMHOTEP00)
### 3.2 [Sea-ice namelist](eORCA025.L75-IMHOTEP00/CTL/namelist_ice.eORCA025.L75-IMHOTEP00)
### 3.3 XIOS control files (xml files)

## 4. Performance optimisation:
A scalability experiment was perfomed on jean-zay, from 240 cores (6 nodes) to 2400 cores (60 nodes). For this experiment, we choose to use 4 xios servers, 
running on a separated computing node. Three-days runs (216 time steps of 1200 sec) were used, and  the last day was taken for evaluating the performance 
(measured as step/mn). On next figure we present the scalability diagram.

<img src=../Doc/Figures/scalability.png  width=130% />

On this busy picture, X-axis  corresponds to the number of cores used for NEMO.  Blue points, corresponds to the actual performance (step/mn, left Y-axis), 
and red points are the equivalent, assuming a perfect scalability (with reference to the 280 cores case). Brown points show the efficiency (%, left Y-axis), 
which is the ratio between actual and theoretical performances.  Except for some outliers, the efficiency is very close to (or above) 100%, until 1800 cores, 
but still very good up to 2400 cores, (85%). According to these performances, yellow points indicates the elapsed time for 1yr of simulation (hours, right 
Y-axis), and green points the CPU hours for 1 year (hoursx10, left Y-axis). Due to the good scalability, the CPU hours are very stable (except for the 
outliers), around 2500 hours/years. In general, the performance decreases somehow when the model spins-up and when the number of icebergs is stabilized. So, 
regarding this experiment, we can estimate  that 1 year of eORCA025 experiment should not cost more than 3250 hours (taking a margin of 30%).

The sweet spot for this experiment is probably around 1500 cores, considering the trade off between queue waiting-time and elapsed time. 

