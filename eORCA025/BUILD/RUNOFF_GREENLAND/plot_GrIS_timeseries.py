#!/usr/bin/env python
# -*- Mode: Python; coding: utf-8; indent-tabs-mode: nil; tab-width: 4 -*-

import os,sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import xarray as xr

def usage():
   print " USAGE: "
   print "     ",os.path.basename(sys.argv[0])," -p INDEX -f GrIS-file -v VARIABLE-name"
   print " "
   print " PURPOSE: "
   print "     Draw a timeserie plot for the given VARIABLE-name in DATA-file as various locations"
   print "     indicated in the POINT-file. "
   print " "
   print " ARGUMENTS:"
   print "    -p INDEX : give the index in the GrIS file to plot "
   print "    -f GrIS-file : give the name of the netcdf data  "
   print "   "
   print " OPTIONS:"
   print "    None so far ..."
   print "   "
   quit()
#------------------------

def timeplot(pds,cd_var, ki, param_dict):
   """
     timeplot : plot time series
     cd_var : name of netcdf variable
     pds    : xarray data set containing cd_var
     ki : index
     param_dict : all the matplotlib options passed to the plot
   """
   zdsloc=pds.isel(name=ki)
   zout=zdsloc[cd_var].plot(**param_dict)
#------------------------

def zparse():
   global  cf_name, cipoint
   cipoint='none'
   cf_name="none"
   ijarg=1
   narg=len(sys.argv)-1
   while ijarg <= narg :
      cldum=sys.argv[ijarg] ; ijarg=ijarg+1
      if cldum == "-p" :
         cipoint=sys.argv[ijarg] ; ijarg=ijarg+1
      elif cldum == "-f":   # just an example
         cf_name=sys.argv[ijarg] ; ijarg=ijarg+1
      else:
         print " +++ ERROR : unknown option ", cldum
         usage()
#----------------
# main program starts here 
cv_lon='lon_nemo'
cv_lat='lat_nemo'

n=len(sys.argv)
if n == 1:
   usage()

zparse()

iipoint=int(cipoint)

# safety check here (none ... )
chk=0

if cf_name == 'none':
   chk=chk+1
   print "+++  ERROR: need a DATA-file (-f option) "

if chk != 0:
   print chk," error(s) found."
   usage()


# read cf_points line per line and process

ds=xr.open_dataset(cf_name)
rlon=ds.data_vars[cv_lon]
rlat=ds.data_vars[cv_lat]

# prepare graph
fig = plt.figure(num = 1, figsize=(12,4), facecolor='w', edgecolor='k')   ; #LB: j'ouvre la figure #1 
ax1 = plt.axes([0.07, 0.2, 0.9, 0.70])                                    ; #LB: occupation de l'espace par la figure
axes=plt.gca()
#axes.set_xlim(20,50)
#axes.set_ylim(36.95,37.15)
axes.yaxis.grid(True)
axes.xaxis.grid(True)
cname="P"+"{:03d}".format(iipoint)
lon=rlon.values[iipoint]
lat=rlat.values[iipoint]
clon="{:5.1f}".format(lon)
clat="{:5.1f}".format(lat)

plot_name=cname+'.png'
plt.suptitle("GrIS data at "+ cname+" "+clon+" "+clat )


print cname, iipoint , clon, clat
timeplot(ds, "rnf_nemo"    , iipoint, {'label': "liquid" })
timeplot(ds, "iceberg_nemo", iipoint, {'label': "calving" })

axes.legend()
#plt.show()

plt.savefig(plot_name, dpi=150, transparent=False)   ; #LB: je sauve, volontier en 'svg' au lieu de 'png' car c'est vectoriel (pour pres ou html...)
plt.close(1)                                           ; # je ferme la figure #1
