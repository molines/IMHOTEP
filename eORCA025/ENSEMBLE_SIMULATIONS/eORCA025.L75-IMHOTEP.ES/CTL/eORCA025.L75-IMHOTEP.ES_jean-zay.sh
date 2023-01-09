#!/bin/bash
#SBATCH --nodes=135
#SBATCH --ntasks=4860
##SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH --hint=nomultithread
#SBATCH -J nemo_ES
#SBATCH -e nemo_jean-zay.e%j
#SBATCH -o nemo_jean-zay.o%j
#SBATCH -A cli@cpu
#SBATCH --time=20:00:00
#SBATCH --dependency=singleton
#SBATCH --exclusive

#for semestre  in {1..2} ; do
for month in {1..16} ; do
#for month in {1..7} ; do
#for month in {1..1} ; do
set -x
ulimit -s 
ulimit -s unlimited

CONFIG=eORCA025.L75
CASE=IMHOTEP.ES

CONFCASE=${CONFIG}-${CASE}
CTL_DIR=$PDIR/RUN_${CONFIG}/${CONFCASE}/CTL
export  FORT_FMT_RECL=255

#  use 480 cores for each member  use 8 xios on 2 nodes per member
# for 2 members 960 core for NEMO 16 Xios on 4 nodes ==> 24 nodes + 4 = 28 nodes
# for 10 members 4800 cores for NEMO 48 xios on 12    ==> 120 nodes + 12 = 132
# for 10 members 4800 cores for NEMO 60 xios on 15    ==> 120 nodes + 15 = 135
# Following numbers must be consistant with the header of this job
export NB_NPROC=4800   # number of cores used for NEMO
export NB_NPROC_IOS=60  # number of cores used for xios (number of xios_server.exe)
export NB_NCORE_DP=4   # activate depopulated core computation for XIOS. If not 0, RUN_DP is
                       # the number of cores used by XIOS on each exclusive node.
# Rebuild process 
#export MERGE=1         # 1 = on the fly rebuild, 0 = dedicated job
#export NB_NPROC_MER=1350 # number of cores used for rebuild on the fly  (1/node is a good choice)
#export NB_NNODE_MER=135  # number of nodes used for rebuild in dedicated job (MERGE=0). One instance of rebuild per node will be used.
export MERGE=0         # 1 = on the fly rebuild, 0 = dedicated job
export NB_NPROC_MER=60 # number of cores used for rebuild on the fly  (1/node is a good choice)
export NB_NNODE_MER=5  # number of nodes used for rebuild in dedicated job (MERGE=0). One instance of rebuild per node will be used.

export WALL_CLK_MER=3:00:00   # wall clock time for batch rebuild
export ACCOUNT=cli@cpu # account to be used

date
#
echo " Read corresponding include file on the HOMEWORK "
.  ${CTL_DIR}/includefile.sh
# copy files not copied in the script so far
cp $P_I_DIR/eORCA025.L75_CT_ref.nc $TMPDIR
cp $P_I_DIR/eORCA025.L75_SA_ref.nc $TMPDIR

. $RUNTOOLS/lib/function_4_all.sh
. $RUNTOOLS/lib/function_4.sh
#  you can eventually include function redefinitions here (for testing purpose, for instance).
. $RUNTOOLS/lib/nemo4.sh
  
  if [ ! -f $TMPDIR/OK.001 ] ; then
     break
  fi
done
