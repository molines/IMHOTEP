#!/bin/bash
date
set -x
########################################################################
#       2. PATHNAME   AND  VARIABLES INITIALISATION                    #
##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
# Some FLAGS (formely deduced from cpp.options) 1= yes, 0= no
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# non standard features (even in DRAKKAR) ( no namelist nor cpp keys defined for that ! ) 
 UBAR_TIDE=0                          # 2D tidal bottom friction
 WAFDMP=0                             # Use WAter Flux DaMPing ( read previous SSS damping climatology in a file)

 RST_SKIP=1                           # if set, checking of the existence of the full set of restart files is disable (save time !)
 # next flags should be set to 1 if using DCM rev > 1674, to 0 otherwise.
 RST_DIRS=1                           # if set, assumes that restart files are written on multiple directories.
 RST_READY=1                          # if set assumes that restart file are ready to be read by NEMO (no links).

#########################################################################

 CONFIG=eORCA025.L75
 CASE=IMHOTEP01
 CONFIG_CASE=${CONFIG}-${CASE}

# Environmemt and miscelaneous
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
login_node=node    # usefull on jade or an any machines requiring scp or ssh to access remote data
#MAILTO=<MAILTO>
ACCOUNT=cli@cpu       # account number for project submission (e.g curie, vayu, jean-zay...)
QUEUE=none         # queue name (e.g. curie )

# Directory names
#~~~~~~~~~~~~~~~~
# 
WORKDIR=$WORK
TMPDIR=$DDIR/TMPDIR_${CONFIG_CASE}
MACHINE=jean-zay

case  $MACHINE  in
( occigen | jean-zay ) SUBMIT=sbatch  ;;
( irene  ) SUBMIT=ccc_msub ;;
( ada    ) SUBMIT=llsubmit ;;
( *      )  echo $MACHINE not yet supported for SUBMIT definition
esac

SUBMIT_SCRIPT=${CONFIG_CASE}_jean-zay.sh   # name of the script to be launched by run_nemo in CTL

if [ ! -d ${TMPDIR} ] ; then mkdir $TMPDIR ; fi

#
# Directory on the storage file system (F_xxx)
F_S_DIR=${SDIR}/${CONFIG}/${CONFIG_CASE}-S       # Stockage
F_R_DIR=${SDIR}/${CONFIG}/${CONFIG_CASE}-R       # Restarts
F_I_DIR=${SDIR}/${CONFIG}/${CONFIG}-I            # Initial + data
F_DTA_DIR=${SDIR}/${CONFIG}/${CONFIG}-I          # data dir
F_FOR_DIR=${SDIR}/DATA_FORCING//DFS5.2_RD/ALL    # in function 3.2
F_OBC_DIR=${SDIR}/${CONFIG}/${CONFIG}-I/OBC      # OBC files
F_BDY_DIR=${SDIR}/${CONFIG}/${CONFIG}-I/BDY      # BDY files
F_MASK_DIR=${SDIR}/${CONFIG}/${CONFIG}-I/MASK    # AABW damping , Katabatic winds
F_INI_DIR=${SDIR}/${CONFIG}/${CONFIG}-I/          
F_WEI_DIR=$SDIR/DATA_FORCING/ERAinterim/ALL

F_OBS_DIR=/ccc/work/cont003/drakkar/drakkar      # for OBS operator
  F_ENA_DIR=${F_OBS_DIR}/ENACT-ENS3
  F_SLA_DIR=${F_OBS_DIR}/j2

# Directories on the production machine (P_xxx)
P_S_DIR=$WORKDIR/${CONFIG}/${CONFIG_CASE}-S
P_R_DIR=$WORKDIR/${CONFIG}/${CONFIG_CASE}-R
P_I_DIR=$WORKDIR/${CONFIG}/${CONFIG}-I                  # mirror on the production machine of the F_I_DIR
P_DTA_DIR=$WORKDIR/${CONFIG}/${CONFIG}-I                # mirror on the production machine of the F_I_DIR
P_FOR_DIR=${WORKDIR}/DATA_FORCING/JRA55/drowned         # forcing files
P_OBC_DIR=${WORKDIR}/${CONFIG}/${CONFIG}-I/OBC          # OBC files
P_BDY_DIR=${WORKDIR}/${CONFIG}/${CONFIG}-I/BDY          # BDY files
P_WEI_DIR=$P_I_DIR

P_CTL_DIR=${PDIR}/RUN_${CONFIG}/${CONFIG_CASE}/CTL      # directory from which the job is  launched
P_CDF_DIR=${PDIR}/RUN_${CONFIG}/${CONFIG_CASE}/CTL/CDF  # directory from which the diags are launched
P_EXE_DIR=${PDIR}/RUN_${CONFIG}/${CONFIG_CASE}/EXE      # directory where to find opa
P_UTL_DIR=${WORKDIR}                                    # root directory of the build_nc programs (under bin )
P_XIOS_DIR=$DEVDIR/xios-2.5-HEAD                        # root directory of the XIOS library and xios_server.exe

P_OBS_DIR=${WORKDIR}/DATA_SET/OBS/                      # for OBS operation
  P_ENA_DIR=${P_OBS_DIR}/ENACT-ENS4.2.1-l09
  P_SLA_DIR=${P_OBS_DIR}/j2

# RUNTOOLS environment is set together with HOMEDCM when installing DCM

# Executable code
#~~~~~~~~~~~~~~~~
EXEC=$P_EXE_DIR/nemo4.exe                              # nemo ...
XIOS_EXEC=$P_XIOS_DIR/bin/xios_server.exe              # xios server (used if code compiled with key_iomput
MERGE_EXEC=$P_UTL_DIR/bin/mergefile_mpp4.exe           # rebuild program (REBUILD_MPP TOOL)  either on the fly (MERGE=1) 
                                                       # or in specific job (MERGE=0). MERGE and corresponding cores number
                                                       # are set in CTL/${SUBMIT_SCRIPT}
                                                       # if you want netcdf4 output use mergefile_mpp4.exe

# In the following, set the name of some files that have a hard coded name in NEMO. Files with variable names
# are directly set up in the corresponding namelist, the script take care of them.
# For the following files, if not relevant set the 'world' name to ''
# set specific file names (data )(world name )                 ;   and their name in NEMO
#--------------------------------------------------------------------------------------------------------
# Ice damping  ! not available in si3 
ICEDMP=                                                        ; NEMO_ICEDMP=ice_damping.nc

# Sub-basin mask for diaptr diags ( mask should include  atlmsk pacmsk indmsk variables
SUBBAS=                                                        ; NEMO_SUBBAS=subbasins.nc

# AHM coef file LDF (dyn)
AHM2D=                                                         ; NEMO_AHM2D=eddy_viscosity_2D.nc
AHM3D=                                                         ; NEMO_AHM3D=eddy_viscosity_3D.nc

# AHT coef file LDF (dyn)
AHT2D=                                                         ; NEMO_AHT2D=eddy_diffusivity_2D.nc
AHT3D=                                                         ; NEMO_AHT3D=eddy_diffusivity_3D.nc

# Tidal mixing (Delavergne)
MXP_BOT=                                                       ; NEMO_MXP_BOT=mixing_power_bot.nc
MXP_PYC=                                                       ; NEMO_MXP_PYC=mixing_power_pyc.nc
MXP_CRI=                                                       ; NEMO_MXP_CRI=mixing_power_cri.nc

DSC_BOT=                                                       ; NEMO_DSC_BOT=decay_scale_bot.nc
DSC_CRI=                                                       ; NEMO_DSC_CRI=decay_scale_cri.nc

# Geothermal flux
GEO=                                                           ; NEMO_GEO=geothermal_heating.nc

# TRACER new CFC file ends in 2005  ( probably obsolete or not up to date )
CFC=                                                           ; NEMO_CFC=cfc1112.atm
CO2=                                                           ; NEMO_CO2=splco2.dat
C14=                                                           ; NEMO_C14=c14.dat

# --- not standard but already used in some config --
# Water flux damping 
WAFDMP_CLIM=ORCA025_wdmp_from_MJM95.nc                         ; NEMO_WAFDMP_CLIM=wdmp_from_MJM95.nc

# ------------------------------------------------------

# Agrif 
# ======
AGRIF_FIXED_GRID=AGRIF_FixedGrids.in                  ; NEMO_AGRIF_FIXED_GRID=AGRIF_FixedGrids.in

# Control parameters
# -----------------
MAXSUB=0                # resubmit job till job $MAXSUB
