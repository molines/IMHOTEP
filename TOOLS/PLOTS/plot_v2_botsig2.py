#!/usr/bin/env python3
# -*- Mode: Python; coding: utf-8; indent-tabs-mode: nil; tab-width: 4 -*-

import os,sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import xarray as xr


fname='OVFLW12.L75-GJM2020_y1983-2011_botsig2.nc'
cvar = 'sobotsigi'

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

lon=rlon.values[x,y]
lat=rlat.values[x,y]

print lon, lat
#
fig = plt.figure(num = 1, figsize=(12,4), facecolor='w', edgecolor='k')   ; #LB: j'ouvre la figure #1 
ax1 = plt.axes([0.07, 0.2, 0.9, 0.70])                                    ; #LB: occupation de l'espace par la figure
plt.suptitle('Bottom sigma2 @ lon='+str(lon)+" lat="+str(lat) )
axes=plt.gca()
#axes.set_xlim(20,50)
axes.set_ylim(36.95,37.15)
axes.yaxis.grid(True)
axes.xaxis.grid(True)

dsloc=ds.isel(x=x,y=y)
dsloc[cvar].plot()                                                        ; #LB: le plot!

dsloc=ds.isel(x=100,y=100)
dsloc[cvar].plot()                                                        ; #LB: le plot!

dsloc=ds.isel(x=75,y=75)
dsloc[cvar].plot()                                                        ; #LB: le plot!

dsloc=ds.isel(x=50,y=50)
dsloc[cvar].plot()                                                        ; #LB: le plot!

plt.savefig(plot_name, dpi=150, transparent=False)   ; #LB: je sauve, volontier en 'svg' au lieu de 'png' car c'est vectoriel (pour pres ou html...)
plt.close(1)                                           ; # je ferme la figure #1
