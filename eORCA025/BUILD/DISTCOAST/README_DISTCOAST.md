# Building the distance to the coast file
  The point is first to build a surface mask where only remains big continents, along which runoff are likely to take place.
For this purpose we use BMGTOOLS interactive editor.

  1. Start from tmask_util from mesh_mask file 

```
    ncks -v nav_lon,nav_lat,tmask_util mesh_mask.nc tmask_util.nc
```

  2. Rename variable tmask_util to Bathymetry (required by BMGTOOLS)

```
   ncrename -v tmask_util,Bathymetry tmask_util.nc
```

  3. Use BMGTOOLS (a full story) ... in order to drown all small islands, ank keep only main continents.
   Once this is done , rename variable Bathymetry to tmask. The resulting file is eORCA025.L75_tmask_distcoast.nc 

  4. Use cdfcofdis for computing the distance to the coast file

```
   cdfcofdis -H eORCA025.L75_domain_cfg_closed_seas.nc  -M eORCA025.L75_tmask_distcoast.nc \
          -T eORCA025.L75_5564_WOA18_1y_vosaline.nc -surf -o eORCA025.L75_distcoast_v1.nc -nc4
```

  5. Edit the discoast_v1.nc file in order to set the Mediteranean, Black sea and Red Sea with a artificial distance of 10000 km.
    This is done playing around with cdfbathy on eORCA025.L75_distcoast_v1.nc.  The idea is that for SSS restoring in these areas
we do not want any fading of the restoring near the coast (as this will almost cancel the restoring almost every where of these narrow areas). 


```
 # Medsea East + Black Sea + Red Sea
 cdfbathy -f eORCA025.L75_distcoast_v2.nc -v Tcoast -zoom 1156 1325 732 920 -sz 10000000 
 # Medsea West (Alboran sea)
 cdfbathy -f eORCA025.L75_distcoast_v2.nc.01 -v Tcoast -zoom 1127 1168 831 873 -sz 10000000 
 # mask the resulting file (restoring islands, but not a pb)
 cdfmltmask -f  eORCA025.L75_distcoast_v2.nc.02 -m ../eORCA025.L75_mesh_mask.nc  -v Tcoast -p T 
 mv eORCA025.L75_distcoast_v2.nc.02_masked eORCA025.L75_distcoast_v2.nc

```

  6.  File eORCA025.L75_distcoast_v2.nc is ready for use, with Tcoast variable
