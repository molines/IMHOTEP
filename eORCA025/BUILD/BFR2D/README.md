# BFR2D file
## 1. Context
  Bottom friction can be locally increased, using a 2D mask file [0-1] indicating the place where this increase takes place. On these areas, bottom friction term is multiplied
by `rn_boost`, if `ln_boost` is true in the namelist block `namdrg_bot`.  `rn_boost` is also defined in the namelist, and we are presently using 50.  The areas where the bottom
friction is increased are :
  * Torres Strait : with the aim of reducing the barotropic flow through the strait
  * Bering Strait : with the aim of reducing the barotropic flow through the strait
  * Bab El Mandeb : with the aim of improving the overflow behaviour
  * Denmark Strait : with the aim of improving the overflow behaviour

For Bab el Mandeb and Denmark strait, only the deepest parts of the straits are concerned by the increase. The idea is to play on the bottom Ekman layer to foster a deep 
cross bathymetry transport.

## 2. Realisation.
We use cdfmkresto CDFTOOLS in order to produce the mask file.  [mkbfr.sh](./mkbfr.sh)  bash script used as a wrapper for cdfmkresto was written for this purpose. 


It was the opportunity to add new capabilities to this cdfmkresto tools. This tool, formely built for creating restoring coefficient file, was designed to create patches of 
rectangular  or circular shape, with a fading out at the boundaries.  In particular, for the circular shape, the decrease of the coefficient started at the very center of 
the circle. For large radius, this was a problem as we were not able to have a constant value on a wide circular area. So we add a third shape for restoring patches, that we
calles 'DISK' shape. For this one, the circular shape is maintained but the value is constant over a prescribed radius, and a linear decrease is applied in a circular rim, whose
width is also prescribed.
