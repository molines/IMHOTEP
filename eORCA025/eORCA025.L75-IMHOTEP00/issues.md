# Running eORCA025.L75-IMHOTEP00
We prepare all the input files as reported in the [**BUILD**](../BUILD) directory. We set up the namelist according to the name of the input files.

## 0. Very first try
  * model blows up at first time step. A look at output.abort.nc shows weird zone where dynamical fields and tracer fields has NaN. The area with NaN
forms large triangle with oblique sides at 45 degree.
  * After checking my log book (!), I have already found this problem due to the use of key_vect_opt_loop
  * unsetting this CPP keys, fix this problem.

## 1. Starting the model
  * model blows up after 12 step.
  * decreasing the time step do not help much
  * changing initial condition change the place of the blow-up
  * eliminating runoff, rnf_isf, calving, geothermal does not change basically.

## 2. Debug.
### 2.1 Baseline gives: (rn_rdt = 1200 seconds)

```
E R R O R
kt 13 |ssh| max   2.004     at i j   1263  875    MPI rank 125
kt 13 |U|   max   13.96     at i j k 1264  884  1 MPI rank 125
kt 13 Sal   min   4.005     at i j k 1197 1042 14 MPI rank 141
kt 13 Sal   max   40.81     at i j k 1360  787 11 MPI rank 105
  ```

  * The model blows at I=1264, J=884 at the surface, sur to high velocities (proc rank 125) (in the Black Sea)
  * A look at output.abort_0125.nc shows indeed high velocities, but almost regular temperature and salinity fields.
  * Weird vertical velocities (but difficult to interpret after so few time steps.

### 2.2 Isolating the  Black sea to make test quicker:
  * need to create a new configuration and retails all input files....
  * dcm_mkconfdir_local BSIMH025.L75-JM01
  * retail with ncks command:

  ```
  ncks -d x,1255,1318 -d y,862,922

  ```

  * script `mkBSIMH.sh`
  *  ==> Good point : reproduce the same error 
  *  ==> Bad point  : dont know why
  * many tests switching on/off diffusion, advection, runoff etc ...
  * ==> kind of magic : error disappear when switching off the restoring (tradmp)

### 2.3 back to eORCA025.L75-IMHOTHEP00
  * Able to start the model.... 
  * Explosion at step 1120 very robust. Disappear when tradmp not used....
  * ==> change restoring dataset from gouretski to WOA18 : OK !
Gouretski, month 2 has probably a problem with spurious bad values ....
    *  Need to rework this data set if we really see an advantage to use it ! I remember that the  original data set has a time dependent land-sea mask
in ice covered areas. I use specific option in SOSIE to deal with this particular case, but it might be possible that the drowning was not satisfactory for those areas (quite large) in
the Wedell sea for instance, at the east of the Antarctic peninsula. 

### 2.4 adding icebergs on the fly
  * With standard code, not possible to start iceberg  in a restart segment: looking for iceberg restart file.
  *  ==> key_drakkar : add  ln_rstart_icb in namberg_drk and replace ln_rstart by ln_rstart_icb in icb_ini.F90

### 2.5 adding iwm (internal wave mixing (Casimir Delavergne ) : `ln_zdfiwm = .true.` in namzdf
  * seems OK so far

### 2.6  All smaller secondary parameterisation (added all at once):
  * adding BFR2D : `ln_boost = .true.` in namdrg_bot
  * adding BBC   : `ln_trabbc = .true.` in nambbc
  * adding BBL   : `ln_trabbl = .true.` in nambbl

## 3. Checking the xml file and prepare a DMP (data management plan).
  * which variable ?
  * which frequency  ?
  * non standard output ? 








