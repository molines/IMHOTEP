#!/bin/bash
#SBATCH --nodes=2
#SBATCH --ntasks=73
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A cli@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_5d_SSH_EAI
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=2:00:00
#SBATCH --exclusive

CONFIG=eORCA025.L75
CASE=IMHOTEP.EAI

CONFCASE=${CONFIG}-${CASE}

narg=$#
if [ $narg = 0 ] ; then
   echo "USAGE : sbatch job5d <YEAR1> <YEAR2> <MBR>"
   exit
fi

YEAR1=$1
YEAR2=$2
MBR=$3
for YEAR in $(seq $YEAR1 $YEAR2) ; do
if [ $(( YEAR % 4 )) = 0  -a $(( YEAR % 400 )) != 0 ] ; then
   leap=1
else
   leap=0
fi

for  typ in icemod flxT gridTsurf ICBT ; do

  DTA1d=$DDIR/$CONFIG/${CONFCASE}.${MBR}-S/1d/$YEAR
  DTA5d=$DDIR/$CONFIG/${CONFCASE}.${MBR}-S/5d/$YEAR
  mkdir -p $DTA5d
  cd $DTA1d
  if [ $leap = 1 ] ; then
    cp $DEVGIT/IMHOTEP/TOOLS/AVERAGE_5d/task_leap.CASE.TYP.MBR.conf ./task.CASE.TYP.MBR.conf
  else
    cp $DEVGIT/IMHOTEP/TOOLS/AVERAGE_5d/task.CASE.TYP.MBR.conf ./task.CASE.TYP.MBR.conf
  fi
  cat task.CASE.TYP.MBR.conf | sed -e "s/<MBR>/$MBR/g"  -e "s/<YEAR>/$YEAR/g" -e "s/<TYP>/$typ/g" -e "s/<CASE>/$CASE/g" > ztask.conf.$$
#  if [ $typ = gridW ] ; then
    cat  ztask.conf.$$ | sed -e "s/-vvl/ /g" > ztmp
    mv ztmp ztask.conf.$$
#  fi
  srun --mpi=pmi2  -m cyclic -K1  --multi-prog  ./ztask.conf.$$

done
done



