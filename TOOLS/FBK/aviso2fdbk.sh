#!/bin/bash

ROOT_DTA=/mnt/meom/DATA_SET/AVISO-1993-2020
SAT_LIST='tp j1 j2 j3'

FDBKDIR=/mnt/meom/workdir/molines/FBK

prog=$DEVGIT/JMMTOOLS/DATA_TOOLS/FBK/aviso_fdbk.exe

mkdir -p $FDBKDIR  ;  cd $FDBKDIR

n=0
for typ in $SAT_LIST ; do
 mkdir -p $FDBKDIR/$typ
 cd $FDBKDIR/$typ
 for f in $ROOT_DTA/$typ/dt*nc ; do
    ln -s $f ./
    g=$( basename $f )
    $prog -f $g -sat $typ  &
    n=$(( n + 1 ))
    if [ $n = 16 ] ; then
       wait
       n=0
       rm dt*nc
    fi
 done
 wait
 rm dt*nc
 n=0
done
wait



