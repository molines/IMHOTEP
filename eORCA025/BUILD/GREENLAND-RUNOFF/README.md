# Making of the Greenland runoff and calving files
## 1. Context

## 2. Programs
### 2.1 Checking
  * **GrIS_chk.f90** : This is a basic reader of the GrIS file. It also build the actual model bathymetry from the mesh-mask file, using `gdepw_0` and `mbathy` variables. Then it performs 2 kinds of check:
    * Localisation of the runoff point in GrIS file (comparing `lon_nemo`, `lat_nemo` with `glamt`, `gphit`).
    * Comparing the actual depth of the model runoff points, and the `rnfdep` variable in the GrIS file. This comparison showed large discrepancies with the initial bathymetry, and pushed us to rebuild a new bathymetry around Greenland, based on recent BedMachine bathymetry. Details of this procedure can be found in this [document](../GREENLAND-BATHY/README.md).
  * **GrIS_chk_bat.f90** : This program only perform the bathymetry comparison, taking the depths from a new test bathymetric file. It allows the correction of some remnant problems in an
iterative way.  Once no more problems are reported the bathymetry is OK and can be merged with the global one.
  * **GrIS_chk_double.f90** : In the GrIS file provides by Jeremie Mouginot et Al., a single NEMO point can receive different runoff/calving contribution. This program detect and list the NEMO points receiving more than one contribution. Decision is to be taken on how the case of multiple source on a single point is treated for the simulation.
