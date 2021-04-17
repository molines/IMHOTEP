#!/bin/bash
# This script is used to eliminate coastal runoff coming from ISBA for
# Antarctica and Greenland coastline. This is only valid for eORCA025 grid.
# In particular, there is a very narrow strait between Greenland and Elsemere
# Island, and masking is done very precisely.
#  Note that variable socoefr is left untouched because there will be runoff there,
# from other sources (Antarctic and GrIs)

CONFIG=eORCA025

AA="     1 1442    1  374"
GRW="  960 1047  971 1142"
GRNE="1017 1076 1028 1157"
GRN1=" 993 1002  986 1148"
GRN2="1005 1023 1140 1150"

rm -f log.f90
for y in {1979..2018} ; do
    cdfvar -f ${CONFIG}_runoff_ISBA_y${y}.nc    -v sorunoff -z $AA   -raz -t 0      # --> 01
    cdfvar -f ${CONFIG}_runoff_ISBA_y${y}.nc.01 -v sorunoff -z $GRW  -raz -t 0 -o   # overwrite 01
    cdfvar -f ${CONFIG}_runoff_ISBA_y${y}.nc.01 -v sorunoff -z $GRNE -raz -t 0 -o
    cdfvar -f ${CONFIG}_runoff_ISBA_y${y}.nc.01 -v sorunoff -z $GRN1 -raz -t 0 -o  
    cdfvar -f ${CONFIG}_runoff_ISBA_y${y}.nc.01 -v sorunoff -z $GRN2 -raz -t 0 -o 
    mv ${CONFIG}_runoff_ISBA_y${y}.nc.01 ${CONFIG}_runoff_ISBA_noAA_noGR_y${y}.nc   # rename 01
done
