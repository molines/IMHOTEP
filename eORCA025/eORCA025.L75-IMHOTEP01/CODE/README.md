# eORCA025.L75-IMHOTHEP01  code
## DCM environment
This configuration is build with DCM/4.0.6 

## Customization and update to DCM
Most of the modifications developped in this configuration were committed and pushed to DCM.
### DIA modules
  * **diaar5.F90** : Modification for saving thermosteric and halosteric 2D SSH
  * **diaprod.F90** : New module for saving second order moments. It is used only if 2nd order moments are to be output (xml files).

### ICB modules
  * **icb_oce.F90**: Declaration of character variables for ICB restart files (in/out) and trajectories.
  * **icbclv.F90** : Optimisation in the computation of `berg_grid%stored_ice`. Standard version produce wrong results on jean-zay with -03 compiler optimization (no clear reason).
  * **icbini.F90** : Allow use of multiple input files for ICB. Allow start of ICB after the first segment.
  * **icbrst.F90** : Allow specification of a directories (in/out) for ICB restart files.
  * **icbstp.F90** : Implementation of multiple input files
  * **icbtrj.F90** : Implement specific directory for ICB trajectory files.

### LBC modules
  * **lib_mpp.F90** : Change in `ctl_opn` for passing optional output directory

### OBS modules
  * **diaobs.F90** : Allow specification of output directory. Bug fix for rn_dobsini, rn_dobsend initialization. (Ticket to NEMO ST to
be done). Note that this initialization assumes that time step is not changed since the begining of the run (ndate0 !) 
  * **obs_profiles_def.F90** : Define output directory for profile OBS files
  * **obs_readmdt.F90** : Set reference level to 0.
  * **obs_surf_def.F90** :  Define output directory for surface OBS files
  * **obs_write.F90** : Implement output directory for OBS files

### SBC modules
  * **sbcblk.F90** : Mask tair and humidity in iom_put
  * **sbcfwb.F90** : Save FW flux correction when using nn_fwb=1
  * **sbcisf.F90** : Allow use of multiple input files for ISF
  * **sbcrnf.F90** : Allow use of multiple input files for RNF

### ZDF modules
  * **zdfiwm.F90** : Modification to read input filenames in the namelist

### ICE modules
  * **icedia.F90** : Change the code for the case ln_hsm=T : bypass reading restart file if the diag activated after the first segment.

## Customization  for IMHOTEP01 configuration:
  * **mppini.F90** : Domain decomposition is done on the base of a  *new* variable `mppmask`, instead of `bottom_level`. These
two variables only differ in the southwest part of the global domain: For eORCA type grid, the domain covers the iceshelves. As we are not using the explicit circulation under the iceshelves, the first ocean points are located around j=150. XIOS domain decomposition is using
zonal stripes on the global domain.  If we use the `bottom_level` variables, the southern most XIOS stripe is not matching any NEMO point
(due to land domain elimination), and the xios server freezes. Some dummy points are added in `mppmask` in order to have at least one
NEMO domain in the southern most XIOS stripe.

  * **obs_prep.F90** : comment out too much information in ocean.output (many tenths of thousand lines!)

## How to build IMOTHEP01 configuration ?
We assume that you have DCM/4.0.6 operational.

### Create Configuration directories:

```
dcm_mkconfdir_local eORCA025.L75-IMHOTEP01
```

### Populate configuration directory with IMHOTEP code

```
cd $UDIR/CONFIG_eORCA025.L75/eORCA025.L75-IMHOTEP01
rsync -arv $DEVGIT/IMHOTEP/eORCA025/eORCA025.L75-IMHOTEP01/CODE/* ./
```

### Install and compile the code
If you are on jean-zay, too easy :

```
cd $UDIR/CONFIG_eORCA025.L75/eORCA025.L75-IMHOTEP01
make install
make
```

If you are on another machine you need to adapt the arch file to the machine and set it in `makefile`


### Prepare the CTL directory

```
cd $PDIR/RUN_eORCA025.L75/eORCA025.L75-IMHOTEP01/CTL
rsync -arv $DEVGIT/IMHOTEP/eORCA025/eORCA025.L75-IMHOTEP01/CTL/* ./
```

### Run the code on jean-zay
  * edit `eORCA025.L75-IMHOTEP01.db` file in order to set the first segment, in CTL directory
  * check the PATHs in includefile.sh
  * check that you have the input data in `$WORK/eORCA025.L75/eORCA025.L75-I`
  * Run the script :

```
./run_nemo.sh
```

In this task list, no doubt that the crucial point is to have the input data ready.
