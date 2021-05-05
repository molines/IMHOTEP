# Making of the Greenland runoff, ice-shelf like and calving files
## 1. Context
Greenland runoff (liquid and solid) were infered from a Greenland Ice Sheet mass balance model (GrIS)
(see for example Mouginot et Al, 2019) and provided to the project by Jeremie Mouginot, Pierre Mathiot and Nicolas Jourdain. 

Main sources of liquid and solid runoff are the glaciers and ice sheets. Some glacier have  a terrestrial end, 
with all the melted waters forming a river that arrives to the Sea. Other glacier have a marine end, with a 
floating ice tongue.  These latter glaciers contribute to both solid (calving of iceberg at the glacier front) 
and liquid (with surface waters and melted water being released  at the bottom of the glacier tongue, at a depth 
depending on the glacier thickness). One of the main problem for forcing an ocean model is that, in case of 
relatively coarse resolution (coarse compared the the fjords width) the first sea-point off the model coastline 
is far from the actual position of the glacier front or the river mouth. More over, a bathymetric sill is very often 
observed downstream fjords. In addition, part of the calved icebergs melt in the fjord before reaching the first 
model sea point !  Considering all these details, some choices have to be made to provide NEMO with liquid discharge, 
deptht of the discharge, and calving rate.  There is a big uncertainty on the amount of freshwater released by 
icebergs that are melting in the fjords.  The litterature on this point (very few indeed), gives for some fjords, 
melted estimates ranging from 30% to 80%  of the calved icebergs. Pragmatically, a conservative choice of 50% have been 
made so far, so that half of the calved iceberg mass is converted to liquid discharge (associated to the glacier 
depth), and the remnant half is used as a calving source at NEMO first sea-point.

## 2. Programs
### 2.1 Checking
  * **GrIS_chk.f90** : This is a basic reader of the GrIS file. It also build the actual model bathymetry from the 
mesh-mask file, using `gdepw_0` and `mbathy` variables. Then it performs 2 kinds of check:
    * Localisation of the runoff point in GrIS file (comparing `lon_nemo`, `lat_nemo` with `glamt`, `gphit`).
    * Comparing the actual depth of the model runoff points, and the `rnfdep` variable in the GrIS file. 
This comparison showed large discrepancies with the initial bathymetry, and pushed us to rebuild a new bathymetry around 
Greenland, based on recent BedMachine bathymetry. Details of this procedure can be found in this 
[document](../GREENLAND-BATHY/README.md).
  * **GrIS_chk_bat.f90** : This program only perform the bathymetry comparison, taking the depths from a new test 
bathymetric file. It allows the correction of some remnant problems in an iterative way.  Once no more problems 
are reported the bathymetry is OK and can be merged with the global one.
  * **GrIS_chk_double.f90** : In the GrIS file provides by Jeremie Mouginot et Al., a single NEMO point can receive 
different runoff/calving contribution. This program detect and list the NEMO points receiving more than one contribution. 
### 2.2 Building RNF ISF and ICB files
  * **GrIS_CreateNemo, GrIS_CreateNemoBis.f90** programs: Spread the GrIS freshwater flux from the database on separate 
files for RNF, ISF and ICB. In the Bis version, 50% of the calving rate is used as liquid freshwater flux for ISF parameterisation. 
All other liquid freshwater fluxes (essentially continental melt waters) are put in the RNF file.
### 2.3 Splitting the file into yearly files, and computing climatology
NEMO requires yearly files. GrIS files provides daily values on the period 1950-2018, and files created by GrIS_CreateNemo 
also have the full period in it.
  * **GrIS_Annual_Split.sh** is a bash script that split the full period files into yearly files.
  * **mk_clim_50-72.sh** is a bash scrpit that compute a daily climatology for the period 1950-1972, that will be used
in the spin-up experiment.


## *References*
**Jérémie Mouginot, Eric Rignot, Anders A. Bjørk, Michiel van den Broeke, Romain Millan, Mathieu Morlighem, Brice Noël, Bernd Scheuchl, Michael Wood (2019)**:
Forty-six years of Greenland Ice Sheet mass balance from 1972 to 2018. *Proceedings of the National Academy of Sciences* May 2019, 116 (19) 9239-9244; DOI: 10.1073/pnas.1904242116

