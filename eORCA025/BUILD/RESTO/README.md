# Restoring coefficient file for Temperature and Salinity 3D restoring (tradmp)
## 1. Context
One of the major flaws of ocean models, is the limitation in the good representation of the overflows. Overflow is a phenomenon which occurs
when dense water is flowing over a bathymetric sill. In the true world, these dense waters tends to pour out downward (gravity effect), entraining
adjacent waters in the motion. The phenomenon is  also influenced by physics due to earth rotation (bottom Ekman layer for instance).

In NEMO (and many other hydrostatic models), the descent of dense water cannot be represented by a vertical acceleration, because the
model is built with the hydrostatic assumption. This missing physics is replaced by vertical mixing (which is also present in the true world via 
the entrainement). Despite many intents, as far as we are using partial cells for vertical coordinates, the overflows are poorly represented,
and this has a strong effect on the caracteristics of deep waters. 

In particular, in the Southern Ocean, the Antarctic Bottom Water (AABW) is fed by very cold and dense waters formed on the Antarctic continental
shelf, through a limited number of sills. With bad overflow representation, the AABW is not fed adequatly, and is eroded  by vertical mixing 
with upper waters. There is a clear negative trend in the AABW volume, with the direct effect of a decrease in the Antarctic Circumpolar 
Current (ACC) transport.  Pragmatically, we decided to use AABW restoring in the deep southern ocean. This is done via Temperature and Salinity restoring 
toward a climatology. The enveloppe of the restored area is defined with a sigma-2, depth and latitude criteria : Area where annual sigma-2 
density is larger than 37.16, depth larger than 1000m and latitude higher than 30 S is restored.  The value of 37.16 was choosen after 
Rintoul (2001), southern ocean water classification, and we use the annual Gouretski (2018) climatology.  The restoring time scale for AABW was 
choosen to be  730 days (2 years). This choice came out from many experiment (Dufour, 2012).

Apart from the AABW restoring described above, we do add  localized TS restoring, in semi enclosed seas such as the Black Sea, the Red Sea and 
the Persian Gulf. For these small inland seas, low resolution atmospheric forcing is not good enough to maintain good water properties. For 
the restoring in semi-enclosed seas, we choose a time scale of 180 days.

Still linked to bad representation of overflows, we decided to add a deep, strong and very localized restoring downsteam of Gibraltar Strait, 
Bab-el-Mandeb and Ormuz Strait. We use time scale of 6 days.   In the case of Gibraltar strait, we know that if this is not done, Mediterranean 
Water stays at about 600 m in the Atlantic instead of 1200m, and this has a real dramatic impact on all the North Atlantic circulation.


## 2. Realisation
Building of the restoring coefficient file (in unit second^-1) is performed using 2 CDFTOOLS : cdfmaskdmp for AABW restoring, then cdfmkresto 
for the other areas. [mkresto.sh](./mkresto.sh) bash script was written as a wrapper and for tracability. It ends up with the creation of 
`eORCA025.L75_resto.nc` file, ready for use in NEMO. (Note that using this resto file, we come back to stantard NEMO code, DRAKKAR 
customization beeing  performed off-line).

Building this file also brings the opportunity to improve `cdfmaskdmp` CDFTOOLS.  We add *-tau* option to pass the restoring time-scale. 
Without this option, the program only produce a continuous mask [0-1] that was formely used in Drakkarized version of NEMO. Without *-tau* 
option, cdfmaskdmp is unchanged.
