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
   * Advantage : this 4.2_RC, have various AGRIF enhancement, of interest for IMOTHEP:
     * AGRIF nests across the periodic boundary
     * AGRIF parallelism for various nests at the same level
     * On the side of advantages, the fact of using a pionnering new version and receive the aknowledgment of 
the communauty ! 
   * Disadvantages : unstable version till the 4.2_RC. And avalanche of bug fixes foreseen during the first 
few month of 4.2_RC.


 Decision : **(to be taken collectively)**

## 2. Preparing Input files
Input data files are independant of the NEMO release to be used (at least I hope so.), and they can be prepared ASAP. 
Among these data files, there are domain configuration file, initial condition files, atmospheric forcing file (or at 
least the corresponing weight files), distance to the coast file for SSS restoring, local enhancements in eORCA025 
(bottom friction, lateral condition free-slip/no-slip), and of course the runoff files, that deserve a detailed work.

### 2.1 Configuration file
This file defines the numerical grid (horizontal and vertical) including the corresponding metrics. In the creation
process we need a coordinate file (for the horizontal grid), a bathymetric file together with a set of streching coeficients
in order to define the vertical grid, with partial cells.  
We will take the coordinates and bathymetry files used by Pierre Mathiot when setting up his eORCA025.L121 configuration.
Vertical stretching parameters will be those of the standard DRAKKAR 75 levels, (as in OCCIPUT, for instance).

### 2.2 Initial conditions files, and restoring files (if any).
There are quite a few available data set for T and S climatology. Among them, the World Ocean Atlases (ex-Levitus), with 
recent releases (last being 2018 release). Also of interest is the EN4 gridded data set, which is probably a bit biased 
toward recent years. There are also many products based on ARGO float remapping.  A choice must be done.  


Also worth to be mentioned is the 3D T-S restoring to be used (or not ?) in some areas. In NEMO (with DRAKKAR enhancement),
initial conditions on T-S and the restoring data set can be different. Traditionnally, in DRAKKAR-like runs, we use to
have restoring on TS, in a very small patch downstream of Gibraltar Strait (in order to restore the Med sea water at
the right depht), but also on a much wider area in the southern Ocean, in the bottom layer corresponding to AABW, south of
30 South (which is defined by a sigma-2 surface from a climatology).  This latter restoring (in the southern ocean) maintains
the AABW which otherwise tend to disappear by vertical mixing and because they are not well alimented by the dense water
overflow off antarctica continental shelf. The consequence of AABW disparition is the spining down of the ACC, which  can be
as low as 95 Sv at Drake passage, after 50 years of run.  If the decision is to keep the AABW restoring we may think about the
data set to use, for this particular restoring. (In the past, we used the Gouretsky annual climatology, which was computed
in a density framework. But this is now a rather old dataset... ). 

### 2.3 Distance-to-the coast file
This file is used for the SSS restoring in order to prevent restoring in the vicinity of the coast, letting the  boundary currents
construct the runoff plumes at the scales allowed by the model resolution (and not present in the restoring data set). 

If this file is simply build from surface *tmask* for instance, some side effects occurs in areas where there are plenty of 
small islands, like in the western tropical pacific: If these islands are taken as land point, a large part of the ocean around
this constellation of islands does not have SSS restoring, which is really bad, because this area is where the correction is
efficient to counter balance poorly represented precipitations !  Hence, the surface *tmask* must be carefully edited, drowning
small islands, in order to have a distance-to-the-coast file that takes into account only main continental land.

### 2.4 Atmospheric forcing files
Decision is to be made on the forcing data set. Candidates are : DFS5.2, JRA55, ERA5. 

### 2.5 Enhancement files
#### 2.5.1 Bottom friction
For eORCA025, we use a locally enhanced bottom friction, in order to control the flow across some specific straits:
Torres Strait, Bering Strait, Gibraltar Strait. This is done through a kind of mask file that defines the areas
where enhancement is applied.

#### 2.5.2 Lateral friction ( shlat2D)
NEMO offers a wide range of lateral boundary conditions from free-slip to 'strong' slip. In NEMO, the lateral condition is 
controlled by the so-called *shlat* coefficient, which can be defined as a 2D field, read in an external file. We use
to have lateral friction modification in some specific straits, (with the same will to reduce the flow, just as we do with 
bottom friction), but also for full sub-areas such as the Mediterranean Sea, along the Antarctic Coast, and for some spots
in the Labrador Sea, along the West Greenland coast.  Playing with this condition is rather empirical, and results from 
many years of model tunning in the communauty. No strict rationale can be provided (intent of defining lateral rugosity
did not lead to convincing results... ). 

### 2.6 Runoff files
This is the central part of IMOTHEP ! For NEMO, the runoff file is a netcdf file on the model grid, with values of the time 
dependent fresh water flux (in kg/m2/s) given for *ad hoc* grid cells. It also contain a mask where *ad hoc* grid cells have
the value of 0.5 instead of 0. elsewhere. 

Building of this file first requires the building of the mask, used to spread the runoff nearby river estuaries. Then, the
values of the runoff are projected on the grid points, according to the area of the model cell. The pre-processing work 
consist of providing timeseries of the runoff for the rivers.  A decision is to be made on the choice of the rivers with interannual
time-series  and those with climatological (seasonal) only time-series.    Once the interannual file is provided,  a
seasonal climatology will be derived.  This climatological file will be used throughout all the 'climatological' experiments, hence
is needed ASAP, for the first run. 

## 3. Preparing run time files: NEMO version dependent.
At run time, NEMO requires control input files such as namelists for the ocean and the sea-ice, as well as a
set of xml files describing the I/O strategy in term of data output for XIOS.
### 3.1 Ocean namelist
### 3.2 Sea-ice namelist
### 3.3 XIOS control files

## 4. Performance optimisation:
Before running a long experiment, it is advisable to perform some scalability tests, in order to decide the number of core 
to be used once in the production phase. These scalability experiments also provide important material to sustain the
HPC proposal to be written every year.  These experiments can be performed before having the correct runoff file.

