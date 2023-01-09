#!/bin/bash

CONFIG=eORCA025.L75

for CASE in EAI EGAI ES ; do
   CONFCASE=${CONFIG}-IMHOTEP.${CASE}
   CTL=$PDIR/RUN_${CONFIG}/${CONFCASE}/CTL
   echo $CASE
   echo =====
   tail $CTL/${CONFCASE}*.db
done
