#!/bin/bash
# WOA2018 processing of raw data files
#  s --> Salinity,  t--> temperatures
#  00 = annual mean
#  01 to 12 = monthly mean
#  13 to 16 = quarterly mean

# keep only the used variables, ie lon,lat, depth, time , s_an or t_an
# remove bounds attributes
# transform time dimension in unlimited dimension and goto netcdf4/hdf5 with deflation of 1
# concatenate monthly files
# concatenate quarterly files

rname=woa18_decav
res=01   #  01 for 1 deg files
         #  04 for 1/4 deg files

# temperatures
for n in {00..16} ; do
  ncks -O -C -v  lon,lat,depth,time,t_an ${rname}_t${n}_${res}.nc ${rname}_tan${n}_${res}.nc
  ncatted -a bounds,,d,, ${rname}_tan${n}_${res}.nc
  ncks -4 -L 1 --cnk_dmn depth,1 --mk_rec_dmn time ${rname}_tan${n}_${res}.nc ${rname}_tan${n}_${res}.nc1
  mv ${rname}_tan${n}_${res}.nc1 ${rname}_tan${n}_${res}.nc
done

# salinity
for n in {00..16} ; do
  ncks -O -C -v  lon,lat,depth,time,s_an ${rname}_s${n}_${res}.nc ${rname}_san${n}_${res}.nc
  ncatted -a bounds,,d,, ${rname}_san${n}_${res}.nc
  ncks -4 -L 1 --cnk_dmn depth,1 --mk_rec_dmn time ${rname}_san${n}_${res}.nc ${rname}_san${n}_${res}.nc1
  mv ${rname}_san${n}_${res}.nc1 ${rname}_san${n}_${res}.nc
done

# concatenate monthly files
for typ in tan san ; do
   ncrcat ${rname}_${typ}0[1-9]_${res}.nc ${rname}_${typ}1[0-2]_${res}.nc ${rname}_${typ}01-12_${res}.nc
done
# concatenate quarterly files
for typ in tan san ; do
   ncrcat ${rname}_${typ}1[3-6]_${res}.nc ${rname}_${typ}13-16_${res}.nc
done
