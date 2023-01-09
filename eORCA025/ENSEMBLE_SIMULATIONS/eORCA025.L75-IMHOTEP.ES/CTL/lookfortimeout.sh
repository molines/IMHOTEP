#!/bin/bash
CONFIG=eORCA025.L75
CASE=IMHOTEP.ES
CONFCASE=${CONFIG}-${CASE}

rundir=$DDIR/TMPDIR_${CONFCASE}

cd $rundir
lst=( $( grep CANCELLED zmergxios.e* | awk -F: '{print $1}' | sort -u ) )


for f in ${lst[@]} ; do
   ls -l $f
   echo -n error for mergxios for segment " "
   grep zXIOS $f | awk -F\. '{print $NF}'
done
#cd -

exit
zmergxios.e753010:slurmstepd: error: *** STEP 753010.9 ON r2i1n8 CANCELLED AT 2022-11-11T18:57:03 DUE TO TIME LIMIT ***
zmergxios.e753010:slurmstepd: error: *** JOB 753010 ON r2i1n8 CANCELLED AT 2022-11-11T18:57:03 DUE TO TIME LIMIT ***
(base) cli [rcli002@jean-zay1: TMPDIR_eORCA025.L75-IMHOTEP.EAI]$ grep CANCEL zmergxios.e* | awk -F: '{print $1}'
zmergxios.e753010
zmergxios.e753010
(base) cli [rcli002@jean-zay1: TMPDIR_eORCA025.L75-IMHOTEP.EAI]$ grep CANCEL zmergxios.e* | awk -F: '{print $1}' | sort -u

+ zXIOS=/gpfsscratch/rech/cli/rcli002/eORCA025.L75-IMHOTEP.EAI-XIOS.257



