# IMHOTEP WP1 runs
## Description:
Numerical experiments defined in the IMHOTEP proposal were performed in August 2021, on jean-zay IDRIS's super computer. Production was done
using the DCM/4.0.6 environment. In this directory, all WPI experiments are collected. Modified FORTRAN code is availbale in CODE sub-sirectories, and
all control files for a run are under CTL sub-directories. This include in particular, namelists and xml files used during run production. The WP1 IMHOTEP run are named
after the nomenclature used in the proposal, with an exception: The spinup run, performed with climatological freshwater fluxes everywhere (and SSS restoring) is IMHOTEP02.
Then the climatological reference run (S) was replayed from 1980-2018, using a E-P interannual monthly correction instead of the SSS restoring.

All experiments were monitored using the Drakkar Monitoring Tools (DMONTOOLS). Result of this monitoring is visible on the [meom monitoring web site](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/eORCA025.L75).  Comparison between 5 WP1 runs is also available on the [COMPARE space](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/COMPARE/eORCA025.L75-IMHOTEP.S_eORCA025.L75-IMHOTEP.GAI_eORCA025.L75-IMHOTEP.AI_eORCA025.L75-IMHOTEP.GA_eORCA025.L75-IMHOTEP.GI/TIME_SERIES/). This monitoring must be enhanced with more specific and local diagnostics to
explore IMHOTEP questions. 


## [IMHOTEP02](./eORCA025.L75-IMHOTEP02) : Spinup run with all freshwater sources set as climatological (and SSS restoring)
  * [monitoring](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/eORCA025.L75/eORCA025.L75-IMHOTEP02)

## [IMHOTEP.S](./eORCA025.L75-IMHOTEP.S) : Experiment with all freshwater sources set as climatological (and __NO SSS restoring__ )
  * [monitoring](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/eORCA025.L75/eORCA025.L75-IMHOTEP.S)

## [IMHOTEP.GAI](./eORCA025.L75-IMHOTEP.GAI) : Experiment with all freshwater sources set as interannual (except Antarctica, always climatological)
  * [monitoring](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/eORCA025.L75/eORCA025.L75-IMHOTEP.GAI)

## [IMHOTEP.AI](./eORCA025.L75-IMHOTEP.AI) : All interannual except Greenland.
  * [monitoring](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/eORCA025.L75/eORCA025.L75-IMHOTEP.AI)

## [IMHOTEP.GA](./eORCA025.L75-IMHOTEP.GA) : All interannual except northern Indian rivers
  * [monitoring](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/eORCA025.L75/eORCA025.L75-IMHOTEP.GA)

## [IMHOTEP.GI](./eORCA025.L75-IMHOTEP.GI) : All interannual except tropical Atlantic rivers
  * [monitoring](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/eORCA025.L75/eORCA025.L75-IMHOTEP.GI)

## [IMHOTEP.GAIa](./eORCA025.L75-IMHOTEP.GAIa) : repeat IMHOTEP.GAI (1997-2007) with monthly climatological E-P correction (instead of interannual in GAI).
  * [monitoring](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/eORCA025.L75/eORCA025.L75-IMHOTEP.GAIa)
  * [comparison](https://ige-meom-drakkar.u-ga.fr/DRAKKAR/COMPARE/eORCA025.L75-IMHOTEP.GAIa_eORCA025.L75-IMHOTEP.GAI) between GAIa and GAI (monitoring).

