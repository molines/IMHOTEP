# TEOS-10 conversion
## 1. Context:
   All temperature and salinity input files must be given with conservative temperatures and absolute salinity, as far as we are using TEOS-10 equation of state.

## 2. Conversion
### 2.1 pt_sp_to_ct_sa program
This program was written to perform conversion from potential temperature, practical salinity file to  conservative temperature, absolute salinity. It is based on the
gsw [toolbox](http://www.teos-10.org/software.htm). This program is developped in the frame of the [DATA_TOOLS](https://github.com/molines/JMMTOOLS/tree/master/DATA_TOOLS).

It can take into account both WOA-like and NEMO-like input files. Usage is as follows:

```
 USAGE :  pt_sp_to_ct_sa  -pt IN-pt -vpt VAR-pt -sp IN-sp -vsp VAR-sp 
          -ct OUT-CT  -vct VAR-CT  -sa  OUT-sa -vsa VAR-SA 
          [-x X-dim ] [-y Y-dim] [-z Z-dim] [-lon VAR-lon] [-lat VAR-lat] 
          [-dep VAR-dep ] [-tim VAR-time] [-2D]
  
  Purpose :
      Compute CT and SA (TEOS10) from potential temperature and practical 
      salinity
  
  Argument:
       -pt  IN-pt  : input potential temperature file
       -vpt VAR-pt : input potential temperature variable
       -sp  IN-sp  : input practical salinity file
       -vsp VAR-sp : input practical salinity  variable
       -ct  OUT-ct : output conservative temperature file
       -vct VAR-ct : output conservative temperature variable
       -sa  PUT-sa : output absolute salinity file
       -vsa VAR-sa : output absolute salinity variable
  
  Options:
       -x X-dim    : X-dimension name [lon]
       -y Y-dim    : Y-dimension name [lat]
       -z Z-dim    : Z-dimension name [depth]
       -t T-dim    : T-dimension name [time]
       -lon VAR-lon  : name of longitude variable [lon]
       -lat VAR-lat  : name of latitude variable [lat]
       -dep VAR-dep  : name of depth variable [depth]
       -tim VAR-time : name of time variable [time]
       -2D           :  use 2D lon lat variables [ F ]
  
  Requested files :
     gsw_data_v3_0.dat : tabulated values for GSW toolbox. 
        Can be found in JMM_TOOLS/DATA_TOOLS/LEVITUS-WOA
```

### 2.2 Wrapper script: [mk_teos10.sh](./mk_teos10.sh)
This bash script can be used for calling the previous program on each pair (potential temperature-practical salinity) of files available.


