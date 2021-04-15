#!/usr/bin/env python
# -*- Mode: Python; coding: utf-8; indent-tabs-mode: nil; tab-width: 4 -*-

import os,sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import xarray as xr

def usage():
   print " USAGE: "
   print "     ",os.path.basename(sys.argv[0])," -p POINT-file -f DATA-file -v VARIABLE-name"
   print " "
   print " PURPOSE: "
   print "     Draw a timeserie plot for the given VARIABLE-name in DATA-file as various locations"
   print "     indicated in the POINT-file. "
   print " "
   print " ARGUMENTS:"
   print "    -p POINT-file : give the name of a POINT-file, decribing the points to sample in DATA-file"
   print "                    for the timeseries plot. POINT-file is a text file (ASCII) with the first line"
   print "                    giving the name of the region, used in the title of the plot. Other lines, "
   print "                    (as many as needed) hold 3 fields : Label  I   J "
   print "                      * I and J are index of the points to plot in the DATA-file."
   print "                      * Label will be used in the legend of the plot."
   print "    -f DATA-file : give the name of the netcdf data file used for plotting. At present, it assumes some"
   print "                   NEMO like characteristics (ie, dimensions x,y, variable nav_lon,nav_lat ... )"
   print "    -v VARIABLE-name : give the name of the netcdf variable to plot. We assume that there is a long_name"
   print "                   attribute, that will be used in the title of the plot."
   print "   "
   print " OPTIONS:"
   print "    None so far ..."
   print "   "
   quit()
#------------------------

def timeplot(pds,cd_var, px, py, param_dict):
   """
     timeplot : plot time series
     cd_var : name of netcdf variable
     pds    : xarray data set containing cd_var
     px, px : location in I J space for the time series
     param_dict : all the matplotlib options passed to the plot
   """
   zdsloc=pds.isel(x=px,y=py)
   zout=zdsloc[cd_var].plot(**param_dict)
#------------------------

def zparse():
   global cf_points, cf_name, cv_name
   cf_points="none"
   cf_name="none"
   cv_name="none"
   ijarg=1
   narg=len(sys.argv)-1
   while ijarg <= narg :
      cldum=sys.argv[ijarg] ; ijarg=ijarg+1
      if cldum == "-p" :
         cf_points=sys.argv[ijarg] ; ijarg=ijarg+1
      elif cldum == "-f":   # just an example
         cf_name=sys.argv[ijarg] ; ijarg=ijarg+1
      elif cldum == "-v":   # just an example
         cv_name=sys.argv[ijarg] ; ijarg=ijarg+1
      else:
         print " +++ ERROR : unknown option ", cldum
         usage()
#----------------
# main program starts here 
cv_lon='nav_lon'
cv_lat='nav_lat'

n=len(sys.argv)
if n == 1:
   usage()

zparse()

# safety check here (none ... )
chk=0
if cf_points == 'none':
   chk=chk+1
   print "+++  ERROR: need a POINT-file (-p option) "

if cf_name == 'none':
   chk=chk+1
   print "+++  ERROR: need a DATA-file (-f option) "

if cv_name == 'none':
   chk=chk+1
   print "+++  ERROR: need a VARIABLE-name (-v option) "

if chk != 0:
   print chk," error(s) found."
   usage()


# read cf_points line per line and process
point=open(cf_points,"r")

# open data file
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
region=point.readline().split()
plot_name=cv_name+'_'+region[0]+'.png'
plt.suptitle(region[0]+" "+str(ds.data_vars[cv_name].attrs['long_name']))

for line  in point:
   lst=line.split()
   cname=lst[0]
   x=int(lst[1])
   y=int(lst[2])

#
   lon=rlon.values[y,x]
   lat=rlat.values[y,x]
   clon="{:5.1f}".format(lon)
   clat="{:5.1f}".format(lat)
   print cname, x, y , clon, clat
   timeplot(ds, cv_name, x,y, {'label': cname })

axes.legend()
#plt.show()

plt.savefig(plot_name, dpi=150, transparent=False)   ; #LB: je sauve, volontier en 'svg' au lieu de 'png' car c'est vectoriel (pour pres ou html...)
plt.close(1)                                           ; # je ferme la figure #1
