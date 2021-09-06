# IMHOTEP02 : IMHOTEP spinup run
## History
After several tests, we arrived to a consensual set up for IMHOTEP's spinup run: eORCA025.L75-IMHOTEP02. Then all sensitivity experiments will use the same code, for the numerics. (Forcing procedure may differ, regarding the experiment). To summarize, this experiment uses the following:
### Numerical schemes
#### Tracers
  * Equation of state : TEOS10
  * Advection : FCT order 4 on horizontal and vertical
  * Diffusion : Harmonic, iso-neutral with scheme 20, meaning 2D scaling of diffusivity, regarding the local horizontal size of the mesh.  
           Lateral diffusive velocity (rn_Ud) : 0.0216 m/s
  * Vertical diffusion : TKE
    * enhanced vertical diffusivity for convection (both tracer and momentum), with a enhanced diffusivity of 10 m2/s.
    * No double diffusion
    * Internal tidal mixing ( De Lavergne ) --> reduction of TKE parameters (rn_emin=1.e-10), and background values (rn_avm0=1.4e-6 m2/s, rn_avt0=1.e-10 m2/s).
Note that we choose a pycnocline-intensidied dissipation that scales as N^2 (Brunt-Vaissala frequency).

  * 3D damping term on TS in critical regions ( some overflow regions --Gibraltar strait, Ormuz strait and Bab-el-Mandeb--, the deepest part of the southern ocean ( in the AABW source area). 
#### Dynamics
  * Advection : Second order vector form scheme, with EEN (energy/enstropy conserving paradigm), Hollingworth correction in the computation of the gradient of the kinetic energy, and using a non masked e3f in the corticity equation.
  * Viscosity: Bi-harmonic horizontal viscosity, scaling wth the cube of the horizontal grid size (scheme 20). rn_uv=0.0614 m/s

#### Forcing
  * We use NCAR bulk formulae (Large and Yeager, 2004), with atmospherical input from JRA55 japonese reananlysis, 3-hourly. 

#### Various:
 *  The OBS (observation operator) is activated so that we produce ENACT-ENSEMBLE observation equivalent during the run.  
Note that activating JASON satelite observation has been forgotten (since 2012)...

## Production of the run:
### Initialization, duration of the run 
 * Run starts January 01, 1958 from rest, using World Ocean Atlas Temperature and Salinity (WOA18)  as initial conditions. Note that the orginal temperatures (in situ) and salinities (practical salinity)  were converted to Conservative Temperature (CT) and Absolute Salinity (SA), in order ro be coherent with the choice of TEOS10 as equation of state.
 * Run ends December 31, 2019.
 * Time step is 1200 seconds throughout the run.
### Archiving strategy.
 * All fields are saved on a daily means and monthly means. This latter field is produced by XIOS and save post processing time needed for average computation. In addition, for monthly mean we save the second order moment, computed every time step. These non linear terms are U2, V2, UT, US, VT,VS, WT and WS.
 
