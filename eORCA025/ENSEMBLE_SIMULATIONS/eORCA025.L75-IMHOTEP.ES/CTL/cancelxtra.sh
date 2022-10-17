#!/bin/bash

qma()  {
squeue -u rcli002  -o  "%.10i %.9P %.15j %.8u %.2t %.10M %.6D %.13l %.13L %20S %r"
       }



lst=( $( qma | grep nemo_GAI | grep PD | awk '{print $1}') )
njob=${#lst[@]}
cancel_id=( ${lst[@]:1:$(( njob - 1 ))} )

echo scancel ${cancel_id[@]}

