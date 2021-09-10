

![IMHOTEP Logo](./Doc/Figures/IMHOTEP_tr.png)



# IMHOTEP : IMpacts of freshwater discHarge interannual variability on Ocean heaT-salt content and rEgional sea level change over the altimetry Period
## Description of IMHOTEP repository
Within IMHOTEP scientific project, numerical experiment based on ocean simulations are performed.  This repository is first of all the place where all 
numerical codes and numerical tools used in the project can be found. From the numerical point of view, 2 different approaches are used:
  1. Global ocean simulations, using different freshwater discharge variability, forming a set of sensitivity experiments.
  2. Ensemble regional simulations (10 members), using the same freshwater forcing  protocol. This second set of experiments is setting up with the
idea of disentangling intrisic and forced variability (that are both present in the turbulent ocean). This allows the quantification of uncertainties and will
help a quantified assesment of the impact of freshwater discharge interannual variability.

## IMHOTEP simulations:
### Global ocean simulations:
IMHOTEP global ocean simulations are realized with the eORCA025.L75 configuration, using NEMO at revision 4.0.6. In the [eORCA025 directory](./eORCA025), a lot
information regarding the setting up of the global configuration can be found.  
Five simulations were performed in this first part of the project, following the OSTST proposal. The name of these experiments are build accordingi to the proposal
nomenclature : `eORCA025.L75-IMHOTEP.<EXP>`  where `<EXP>` can be one of S, GAI, AI, GA or GI, each case corresponding to a specific set of freshwater discharge forcing.
The spinup run (used for spinning up the model to some equilibrium state) is also reported and its name is eORCA025.L75-IMHOTEP02.

For these 5 simulations, numerical code (modified NEMO's fortran modules)  as well as control files (namelists, xml files for  XIOS) are given. 

### Ensemble regional simulations:
These simulations are to be set up in 2022-2023. It is an on going work


This is a work in progress. Do not use  at this stage ! 

