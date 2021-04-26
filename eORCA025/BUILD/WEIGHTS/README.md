# Making of weight files for Interpolation On the Fly (IOF)
## 1. Context
Interpolation on the fly is a very clever process introduced in NEMO 2.4. It allows the use of data set on their original grid, which are interpolated on the
model grid when needed.  This is particularly usefull for atmospheric forcing for instance.  The interpolation can be performed by either a bilinear approach or a
bicubic approach.  For the atmospheric forcing field, we adopt bilinear interpolation for all fields except for wind velocity component, where we prefer bicubic 
interpolation. This is basically because, one important quantity is the curl of the wind stress, and with a bilinear interpolation, this field is not continuous and
shows step values corresponding to the atmospheric grid resolution (coarse). Here, we describe the procedure used to produce the weight files.

## 2. Preparation
  * Need to compile the WEIGHTS tools :

``` 
   cd $UDIR/CONFIG_eORCA025.L75/eORCA025.L75-IMHOTEP00/
   dcm_mktools -n WEIGHTS -m X64_JEANZAY_jm -c eORCA025.L75-IMHOTEP00

```  

  * And make them available in the build directory

```  
   mkdir -p  $WORK/eORCA025.L75/eORCA025.L75-I/WEIGHTS_build
   cd /gpfswork/rech/cli/rcli002/WeORCA025.L75-IMHOTEP00/tools/WEIGHTS
   cp namelist.skel mkweight.ksh /gpfswork/rech/cli/rcli002/eORCA025.L75/eORCA025.L75-I/WEIGHT_build 
   ln -sf /gpfswork/rech/cli/rcli002/WeORCA025.L75-IMHOTEP00/tools/WEIGHTS/BLD/bin/*exe ./
```  

## 3. Compute the weight file  for  bilinear and bicubic interpolation (for forcing fields)
  * need the horizontal grid (in eORCA025.L75_mesh_mask.nc)
  * need a forcing file for the atmospheric grid (e.g. drowned_psl_JRA55_y2000.nc) (of course if we are to use JRA55).
  * use the mkweight script :

```
   ./mkweight.ksh -c eORCA025.L75_domain_cfg_closed_seas.nc  -M drowned_psl_JRA55_y2000.nc  -m bilinear
   ./mkweight.ksh -c eORCA025.L75_domain_cfg_closed_seas.nc  -M drowned_psl_JRA55_y2000.nc  -m bicubic
   mv wght_bicubic_eORCA025.L75_domain_cfg_closed_seas.nc  ../wght_JRA55_eORCA025_bicub.nc
   mv wght_bilinear_eORCA025.L75_domain_cfg_closed_seas.nc ../wght_JRA55_eORCA025_bilin.nc
   
```
> Tip : in the mkweight.ksh script, I set BINDIR=./ 

## 4. Compute the weight file for the geothermal flux (Goutorbe)
  * need the Geothermal flux file ( ghflux_v2.0.nc  )
  * use the mkweight script

```
   ./mkweight.ksh -c eORCA025.L75_domain_cfg_closed_seas.nc  -M ghflux_v2.0.nc  -m bilinear
   mv wght_bilinear_eORCA025.L75_domain_cfg_closed_seas.nc ../wght_ghflux_eORCA025_bilin.nc
```

