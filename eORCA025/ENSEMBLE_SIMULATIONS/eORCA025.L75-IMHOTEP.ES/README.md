# eORCA025.L75-IMHOTEP.ES configuration

## Code :
  * NEMO 4.0.6
  * DCM : 4.0.6ens
  * XIOS : 
     * URL: http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-2.5
     * Revision: 1869

## Production of IMHOTEP.ES:
  * Start date : 1975.01.01 from IMHOTEP.S restart
  * 10 members initialized with same restart
  * Year 1975 perfomed with stochastic parameterization on equation of state :
  
  ```
  !-----------------------------------------------------------------------
  &namsto        ! Stochastic parametrization of EOS                      (default: OFF)
  !-----------------------------------------------------------------------
     ln_sto_eos  = .true.   ! stochastic equation of state
     nn_sto_eos  = 1         ! number of independent random walks
     rn_eos_stdxy = 0.5      ! random walk horz. standard deviation (in grid points)
     rn_eos_stdz = 0.25      ! random walk vert. standard deviation (in grid points)
     rn_eos_tcor = 108.      ! random walk time correlation (in timesteps)
     nn_eos_ord  = 1         ! order of autoregressive processes
     nn_eos_flt  = 0         ! passes of Laplacian filter
     rn_eos_lim  = 2.0       ! limitation factor (default = 3.0)
     ln_rststo   = .false.   ! start from mean parameter (F) or from restart file (T)
     ln_rstseed  = .true.    ! read seed of RNG from restart file
     cn_storst_in  = "restart_sto" !  suffix of stochastic parameter restart file (input)
     cn_storst_out = "restart_sto" !  suffix of stochastic parameter restart file (output)
  /
  ```

  * Years 1976  to end (wip) : no stochastic param
  * Forcing settings identical to IMHOTEP.S ( all runoff/calving/ice_shelf melting climqtological)
  * Each member is forced by the ensemble mean fluxes
