# eORCA025.L75-IMHOTHEP01  code
## DCM environment
This configuration is build with DCM/4.0.6 

## Customization and update to DCM
Most of the modifications developped in this configuration were committed and pushed to DCM.
### DIA modules
  * **diaar5.F90** : Modification for saving thermosteric and halosteric 2D SSH
  * **diaprod.F90** : New module for saving second order moments.

### ICB modules
  * **icb_oce.F90**:
  * **icbclv.F90** :
  * **icbini.F90** :
  * **icbrst.F90** :
  * **icbstp.F90** :
  * **icbtrj.F90** :

### OBS modules
  * **diaobs.F90** : Allow specification of output directory. Bug fix for rn_dobsini, rn_dobsend initialization. (Ticket to NEMO ST to
be done).
  * **obs_profiles_def.F90** :
  * **obs_readmdt.F90** :
  * **obs_surf_def.F90** :
  * **obs_write.F90** :

### SBC modules
  * **sbcblk.F90** : Mask tair and humidity in iom_put
  * **sbcfwb.F90** : Save FW flux correction when using nn_fwb=1
  * **sbcisf.F90** : Allow use of multiple input files for ISF
  * **sbcrnf.F90** : Allow use of multiple input files for RNF

### ZDF modules
  * **zdfiwm.F90** : Modification to read input filenames in the namelist

## Customization  for IMHOTEP01 configuration:
  * **mppini.F90** : Domain decomposition is done on the base of a  *new* variable `mppmask`, instead of `bottom_level`. These
two variables only differ in the southwest part of the global domain: For eORCA type grid, the domain covers the iceshelves. As we are not using the explicit circulation under the iceshelves, the first ocean points are located around j=150. XIOS domain decomposition is using
zonal stripes on the global domain.  If we use the `bottom_level` variables, the southern most XIOS stripe is not matching any NEMO point
(due to land domain elimination), and the xios server freezes. Some dummy points are added in `mppmask` in order to have at least one
NEMO domain in the southern most XIOS stripe.

  * **obs_prep.F90** : comment out too much information in ocean.output (many tenths of thousand lines!)
