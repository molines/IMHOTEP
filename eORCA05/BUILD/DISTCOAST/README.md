# Distance to coast file
This file is used in SSS restoring. The idea is to avoid SSS restoring near the coast, where coastal
currents can build localy fine stuctures on the SSS, in particular because of the river runoff entering
the ocean. SSS restoring is done toward SSS climatologies that are very smooth and absolutly not 
resolving small scale coastal features.  

When using this DRAKKAR option of NEMO, SSS restoring is faded out to 0 near the coast. The fading-out 
typical lenght scale is around 100 km and defined in the namelist.

For IMHOTEP, as the main goal is to investigate the effect of the interannual river runoff, the 
distance to the coast file is build using all the coastal pointscoming out from the runoff file.

The script [caldist_from_rnf.sh](./caldist_from_rnf.sh) has been used, based on cdfcofdis CDFTOOLS. 
Some semi enclosed seas like Mediterranean sea, Black Sea, Red Sea and Persian gulf, require restoring
for the lack of good air-sea fluxes.  In the script, we use cdfbathy to patch  a very large distance 
on these areas so that full SSS restoring is applied. Nevertheless, no detailed studies relative 
to these areas has been performed so far. And this patching may be localy replaced by using a smaller
typical fading-out length scale.

