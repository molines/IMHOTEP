#!/bin/bash

CONFCASE=eORCA025.L75-IMHOTEP.ES

CONFIG=${CONFCASE%-*}

year=1980
nblk=73
leap=0
if [ $(( year % 4 )) = 0 -a $(( year % 400 )) != 0 ]  ; then    leap=1; fi


for typ in gridU gridV gridW gridT ; do
  for mbr in {001..010} ; do

     SSDIR=$DDIR/$CONFIG/${CONFCASE}.${mbr}-S/1d/$year
     cd $SSDIR
     mkdir -p ../../5d/$year

     file_lst=( $(ls -1 ${CONFCASE}.${mbr}_y${year}m??d??.1d_${typ}.nc ) )

     nfil=${#file_lst[@]}

     echo $nfil files of $typ type

     for n in $(seq 0 $(( nfil - 1 )) ) ; do
       dat[$n]=$( echo ${file_lst[$n]} | awk -F_ '{print $2}' | awk -F. '{print $1}'  )
     done

     ib=0
     for n in $(seq 0 $(( nblk - 1 )) ) ; do
        dat_lst_5d[$n]="${dat[$ib]}"
        file_lst_5d[$n]="${file_lst[$ib]}"
        if [ $n = 11 -a $leap = 1 ] ; then 
           nsz=6
        else
           nsz=5
        fi
        for  ik in $( seq 1 $(( nsz - 1 )) ) ; do
           ib=$(( ib + 1 ))
           dat_lst_5d[$n]="${dat_lst_5d[$n]} ${dat[$ib]}"
           file_lst_5d[$n]="${file_lst_5d[$n]} ${file_lst[$ib]}"
        done
       ib=$(( ib + 1 ))
       echo "$n : ${dat_lst_5d[$n]}"
    done

    rm -f ztask.${typ}.$mbr.conf
    for n in $( seq  0 $(( nblk - 1 )) ) ; do
      ztmp=$( echo  ${file_lst_5d[$n]%.nc} | awk '{print $NF}' )
      ztmp2=$( echo $ztmp | sed -e 's/.1d/.5d/')
      g="../../5d/$year/$ztmp2"
      echo ${n}-${n} cdfmoy -l ${file_lst_5d[$n]} -vvl -nc4 -o $g  >> ztask.${typ}.$mbr.conf
    done
  done
done
