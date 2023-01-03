#!/usr/bin/env python3
# -*- Mode: Python; coding: utf-8; indent-tabs-mode: nil; tab-width: 4 -*-

import os,sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import xarray as xr

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


fname='OVFLW12.L75-GJM2020_y1983-2011_botTS.nc'
cvar = 'votemper'

n=len(sys.argv)
if n < 2:
   print "USAGE :", os.path.basename(sys.argv[0]), " i  j "
   quit()

x=int(sys.argv[1])
y=int(sys.argv[2])
plot_name=cvar+'_'+str(x)+'_'+str(y)+'.png'
print plot_name

ds=xr.open_dataset(fname)
rlon=ds.data_vars['nav_lon']
rlat=ds.data_vars['nav_lat']

#
fig = plt.figure(num = 1, figsize=(12,4), facecolor='w', edgecolor='k')   ; #LB: j'ouvre la figure #1 
ax1 = plt.axes([0.07, 0.2, 0.9, 0.70])                                    ; #LB: occupation de l'espace par la figure
plt.suptitle('Bottom theta0' )
axes=plt.gca()
#axes.set_xlim(20,50)
#axes.set_ylim(36.95,37.15)
axes.yaxis.grid(True)
axes.xaxis.grid(True)

lon=rlon.values[x,y]
lat=rlat.values[x,y]
clon="{:5.1f}".format(lon)
clat="{:5.1f}".format(lat)
timeplot(ds, cvar, x,y, {'label':'A' })

x=179
y=163
lon=rlon.values[x,y]
lat=rlat.values[x,y]
clon="{:5.1f}".format(lon)
clat="{:5.1f}".format(lat)
timeplot(ds, cvar, x,y, {'label':'B' })


x=143
y=130
lon=rlon.values[x,y]
lat=rlat.values[x,y]
clon="{:5.1f}".format(lon)
clat="{:5.1f}".format(lat)
timeplot(ds, cvar, x,y, {'label':'C' })

x=109
y=99
lon=rlon.values[x,y]
lat=rlat.values[x,y]
clon="{:5.1f}".format(lon)
clat="{:5.1f}".format(lat)
timeplot(ds, cvar, x,y, {'label':'D' })

x=70
y=53
lon=rlon.values[x,y]
lat=rlat.values[x,y]
clon="{:5.1f}".format(lon)
clat="{:5.1f}".format(lat)
timeplot(ds, cvar, x,y, {'label':'E'})

axes.legend()
#plt.show()

plt.savefig(plot_name, dpi=150, transparent=False)   ; #LB: je sauve, volontier en 'svg' au lieu de 'png' car c'est vectoriel (pour pres ou html...)
plt.close(1)                                           ; # je ferme la figure #1
