#!/bin/bash
for f in EN.4.2.1.f.profiles.l09.??????.nc ; do
   out=$( echo $f | sed -e 's/.nc/_fdbk.nc/')
   ./enact2fb.exe $out $f
done


exit
EN.4.2.1.f.profiles.l09.195701.nc  EN.4.2.1.f.profiles.l09.195708.nc  EN.4.2.1.f.profiles.l09.195803.nc  EN.4.2.1.f.profiles.l09.195810.nc
EN.4.2.1.f.profiles.l09.195702.nc  EN.4.2.1.f.profiles.l09.195709.nc  EN.4.2.1.f.profiles.l09.195804.nc  EN.4.2.1.f.profiles.l09.195811.nc
EN.4.2.1.f.profiles.l09.195703.nc  EN.4.2.1.f.profiles.l09.195710.nc  EN.4.2.1.f.profiles.l09.195805.nc  EN.4.2.1.f.profiles.l09.195812.nc
EN.4.2.1.f.profiles.l09.195704.nc  EN.4.2.1.f.profiles.l09.195711.nc  EN.4.2.1.f.profiles.l09.195806.nc  EN.4.2.1.profiles.l09.download-list_1957-2021.txt
EN.4.2.1.f.profiles.l09.195705.nc  EN.4.2.1.f.profiles.l09.195712.nc  EN.4.2.1.f.profiles.l09.195807.nc  README
EN.4.2.1.f.profiles.l09.195706.nc  EN.4.2.1.f.profiles.l09.195801.nc  EN.4.2.1.f.profiles.l09.195808.nc  ZIP
EN.4.2.1.f.profiles.l09.195707.nc  EN.4.2.1.f.profiles.l09.195802.nc  EN.4.2.1.f.profiles.l09.195809.nc  enact2fb.exe

