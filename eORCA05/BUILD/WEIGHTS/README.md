# Making of weight files for forcing

> This is exactly the same procedure that was used for eORCA025, just the file names are changed !

## Preparation
  * Need to compile the WEIGHTS tools :

``` 
   cd $UDIR/CONFIG_eORCA05.L75/eORCA05.L75-JZ406/
   dcm_mktools -n WEIGHTS -m X64_JEANZAY_jm -c eORCA05.L75-JZ406

```  

  * And make them available in the build directory

```  
   mkdir -p  $WORK/eORCA05.L75/eORCA05.L75-I/WEIGHTS_build
   cd /gpfswork/rech/cli/rcli002/WeORCA05.L75-JZ406/tools/WEIGHTS
   cp namelist.skel mkweight.ksh /gpfswork/rech/cli/rcli002/eORCA05.L75/eORCA05.L75-I/WEIGHT_build 
   ln -sf /gpfswork/rech/cli/rcli002/WeORCA05.L75-JZ406/tools/WEIGHTS/BLD/bin/*exe ./
```  

## Compute the weight file  for  bilinear and bicubic interpolation
  * need the horizontal grid (in eORCA05.L75_mesh_mask.nc)
  * need a forcing file for the atmospheric grid (e.g. drowned_psl_JRA55_y2000.nc) (of course if we are to use JRA55).
  * use the mkweight script :

```
   ./mkweight.ksh -c eORCA05.L75_domain_cfg.nc  -M drowned_psl_JRA55_y2000.nc  -m bilinear
   ./mkweight.ksh -c eORCA05.L75_domain_cfg.nc  -M drowned_psl_JRA55_y2000.nc  -m bicubic
   mv wght_bicubic_eORCA05.L75_domain_cfg.nc  ../wght_JRA55_eORCA05_bicub.nc
   mv wght_bilinear_eORCA05.L75_domain_cfg.nc ../wght_JRA55_eORCA05_bilin.nc
   
```
> Tip : in the mkweight.ksh script, I set BINDIR=./ 

## compute the weight file for the geothermal flux (Goutorbe)
  * need the Geothermal flux file ( ghflux_v2.0.nc  )
  * use the mkweight script

```
   ./mkweight.ksh -c eORCA05.L75_domain_cfg.nc  -M ghflux_v2.0.nc  -m bilinear
   mv wght_bilinear_eORCA05.L75_domain_cfg.nc ../wght_ghflux_eORCA05_bilin.nc
```

