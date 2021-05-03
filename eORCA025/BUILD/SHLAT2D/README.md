# Lateral boundary condition enhancement : shlat2D
## 1. Context
Lateral boundary conditions are dealing with the `no-slip`  vs `free-slip` condition. This represents the assumption whether the tangential coastal velocity vanish at the coast (no-slip) or is unaffected (free-slip).  This is a way to define the lateral boundary layer, which obviously depends on the model resolution. 
In NEMO, we can define  lateral boundary condition from strong-slip to free-slip (passing through no-slip). 

In fact, on the C-grid (see figure below), there are no 'tangential' velocity point on the coast line. If we take the example of a N-S coast, there are U velocity points on 
the coastal mask (set to 0, normal velocity). The V points are located half a grid cell off the coast.  The lateral boundary condition is implemented in the way the
vorticity component  dV/dx is computed at the coastal F-point: In case of free-slip, dv/dx=0 meaning no variation of V when getting near the coast. In case of no-slip,
V@F-points should be 0 (which can be represented if the mirror V-point in land (symetric to F-point) has -V value, giving a 0 value at the middle F-point. 
Thus, in this case, the vorticity component is (V - (-V))/dx = 2 V/dx.

<img src=./C-Grid.jpg  width=40% />


NEMO implementation introduces a namelist coefficient `shlat` which  is used in the computation of the vorticity component : dv/dx=  shlat * V /dx (V being the first 
in sea, tangential velocity point). Hence, shlat=0 corresponds to free-slip and shlat= 2 corresponds to no-slip. But the concept of strong slip (shlat > 2) can be
imagined: it just increases the vorticity at the 
coast (in order to mimic a thinner lateral boundary layer).

In the DRAKKAR version of NEMO, we decided to use a 2D field for the shlat coefficient, because we want to use localized no-slip or strong-slip 
lateral boundary conditions.  The 2D coefficient is stored in the file whose building is decribed here.


## 2. Realisation
The shlat2d  file is built using cdfmkresto CDFTOOL. [mkshlat2d.sh](./mkshlat2d.sh) bash script was written as a wrapper of cdfmkresto. This is also a good way to ensure tracability.
Note that shlat2d grid corresponds to F-points. 

The main lines  are : free-slip everywhere except 
  * Bering Strait : we implement no-slip in order to reduce the inflow in the Arctic
  * Whole Mediterranean sea : we set no-slip for all the area, following advices of Med Sea experts (UIB). The rational for this is not clear to my mind, although it has a great impact on 
the Alboran gyres. 
  * Along West Greenland coast: Setting a stripe of no-slip condition, helps the destabilization of Western Greenland Current (WGC), and the shedding of eddies at Cape Desolation, which is
observed (for instance on EKE derived from satelite altimetry). 
  * In historical DRAKKAR configurations, shlat was modified in some indonesian straits (Lombok and Ombai). We add this customization in the SHLAT2D file.  For this customization,
cdfmkresto was slightly modified in order to deal with rectangular shape patch (I) given with the I,J coordinates of the horizontal grid.
