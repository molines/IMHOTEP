# Making of weight files for forcing

## Preparation
  * Need to compile the WEIGHTS tools :

``` 
   cd $UDIR/CONFIG_eORCA025.L75/eORCA025.L75-IMOTHEP00/
   dcm_mktools -n WEIGHTS -m X64_JEANZAY_jm -c eORCA025.L75-IMOTHEP00

```  

  * And make them available in the build directory

```  
   mkdir -p  $WORK/eORCA025.L75/eORCA025.L75-I/WEIGHTS_build
   cd /gpfswork/rech/cli/rcli002/WeORCA025.L75-IMOTHEP00/tools/WEIGHTS
   cp namelist.skel mkweight.ksh /gpfswork/rech/cli/rcli002/eORCA025.L75/eORCA025.L75-I/WEIGHT_build 
   ln -sf /gpfswork/rech/cli/rcli002/WeORCA025.L75-IMOTHEP00/tools/WEIGHTS/BLD/bin/*exe ./
```  

## Compute the weight file  for  bilinear and bicubic interpolation
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

