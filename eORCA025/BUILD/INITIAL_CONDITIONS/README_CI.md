# Building Initial condition step by step

## Getting the climatological data
### Recent version of World Ocean Atlas : WOA2018
  * We took the TS climatology from the NOAA data center. 
  * This raw data set requires some adaptation before using it for interpolation.
  * See details of the preparation in this [report](../WOA2018/WOA18_processing.md)

The building of initial conditions suppose that you already have :
  * domain_cfg file for the configuration
  * mesh_mask files for the configuration
  * TS 3D-data set that will be interpolated to the model grid
  * A last version of [SOSIE3](https://github.com/brodeau/SOSIE), ready to be used. 

For eORCA025.L75-IMOTHEP00 we have :
|  files        | name                                   |
| ------------- |:--------------------------------------:|
| domain_cfg    | eORCA025.L75_domain_cfg_closed_seas.nc |
| mesh_mask     | eORCA025.L75_mesh_mask.nc              |
| T-file        |                                        |
| S-file        |                                        |

> TIP : One the domain_cfg file is OK, the best way to obtain a `mesh_mask.nc` file is to run NEMO with `ln_meshmask=.true.` in the namelist
> this job will crash almost immediatly because of so many missing files ... but at least will produce the mesh_mask.nc file !

## Using Sosie for T and S
