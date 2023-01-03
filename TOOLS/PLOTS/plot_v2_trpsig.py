#!/usr/bin/env python3
# -*- Mode: Python; coding: utf-8; indent-tabs-mode: nil; tab-width: 4 -*-

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import xarray as xr


fname='01_Denmark_strait_y1980-2011_trpsig_above_27_8.nc'

ds=xr.open_dataset(fname)

dsloc=ds.isel(x=0,y=0)

cvar = 'sum_3D_tsigtrp'

fig = plt.figure(num = 1, figsize=(12,4), facecolor='w', edgecolor='k')   ; #LB: j'ouvre la figure #1 
ax1 = plt.axes([0.07, 0.2, 0.9, 0.75])                                    ; #LB: occupation de l'espace par la figure

dsloc[cvar].plot()                                                        ; #LB: le plot!

plt.savefig(cvar+'.png', dpi=150, transparent=False)   ; #LB: je sauve, volontier en 'svg' au lieu de 'png' car c'est vectoriel (pour pres ou html...)
plt.close(1)                                           ; # je ferme la figure #1
