#!/bin/bash

if [ $# = 0 ] ; then
   echo "  USAGE : mkmp4 NAME_id"
   echo "    Purpose :"
   echo "      Transform a bunch of sequential png file into an mp4 movie."
   echo "    Options :"
   echo "      NAME_id : a name for the final output mp4 file"
   echo "      video name is movie_NAME_id_x264_1080px_12fps_crf20.mp4"
   echo " "
   exit
fi

NAME=$1

n=0
for f in *.png ; do
   n=$(( n + 1 ))
   nnnn=$( printf "%04d" $n )
   ln -sf $f ${NAME}_$nnnn.png
done

images2mp4.sh -i $NAME

rm ${NAME}_????.png




