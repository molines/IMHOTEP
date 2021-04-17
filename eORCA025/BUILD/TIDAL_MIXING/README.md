# Internal Wave Mixing (IWM)
## 1. Context
We use the internal wave mixing parametrization, developed by Casimir de Lavergne. This parametrization
produce a vertical mixing coeffficient that takes into account the dissipation introduced by internal tides.
We follow Casimir's advice to use a pycnocline intensified dissipation that scales with N²(setting `nn_zpyc` = 2
in the namelist).

## 2. Data set:
This parametrization requires 5 input 2D variables (decay_scale_cri, decay_scale_bot, mixing_power_cri, mixing_power_pyc, mixing_power_bot).
Casimir provides these fields on a regular 0.5 degree grid. (see ige-meom-cal1:/mnt/meom/DATA_SET/NEW_TMX_Casimir_de_Lavergne)

For this eORCA025 run, we take the already interpolated file, prepared by Pierre Mathiot and whose name is `eORCA025_iwm_b0.2_v0.0`.

This file is kept on the meom opendap and can be retrieved with the [get_tmxfile.sh](./get_tmxfile.sh). 
