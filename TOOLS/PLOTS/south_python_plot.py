#!/usr/bin/env python
"""
   This program is the first stone of a much more ambitious project aiming at replacing
   chart and coupe (born in the late 80's) by a python engine, keeping the same spirit
   and trying to reproduce the same options.... Ambitious indeed ...
   Need to perform conda activate BASEMAP on JZ
"""
# Module required for this program
import sys
from os import path, getcwd, mkdir
from string import replace
import argparse as ap
import numpy as nmp

from netCDF4 import Dataset

import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.image as image
import matplotlib.cbook as cbook

from mpl_toolkits.basemap import Basemap
from mpl_toolkits.basemap import shiftgrid

from calendar import isleap
import datetime

from re import split
import warnings

import datetime
import netcdftime as nctime

##
warnings.filterwarnings("ignore")


# default values

#  ARGUMENT PARSING
parser = ap.ArgumentParser(description='Generate pixel maps of a given scalar.')
#  Required
requiredNamed = parser.add_argument_group('required arguments')
requiredNamed.add_argument('-i', '--input_file', required=True,                help='specify the input file ...')
requiredNamed.add_argument('-v', '--variable'  , required=True,                help='specify the input variable')
requiredNamed.add_argument('-wij', '--ijwindow', required=True, nargs=4,       help=' data window in i,j coordinate (imin jmin imax jmax)' )
requiredNamed.add_argument('-wlonlat', '--lonlatwindow', required=True, nargs=4, help=' plot window in lonlat coordinate (lonmin latmin lonmax latmax)' )
#  Options
parser.add_argument('-p', '--palette',default='plasma',required=False,         help='specify the palette name')
parser.add_argument('-d', '--figures',default='./figs',required=False,         help='specify the output directory')
parser.add_argument('-t', '--frame',  type=int, default=-1,required=False,     help='specify the beg frame  number, [all]')
parser.add_argument('-nt', '--number',type=int, default=-1,required=False,     help='specify the number of frame, [all]')
parser.add_argument('-dpi','--dpi',type=int,    default=150,required=False,    help='specify dpi resolution [150] ')
parser.add_argument('-proj','--projection',     default='cyl',required=False,    help='specify projection [cyl] ')
parser.add_argument('-xstep','--xstep',type=float, default=45.0,required=False,    help='specify longitude graduation [45] ')
parser.add_argument('-ystep','--ystep',type=float, default=45.0,required=False,    help='specify latitude graduation [45] ')

args = parser.parse_args()
####

cf_in=args.input_file
cv_in=args.variable
cmap=args.palette
frame=args.frame
number=args.number
dpi=args.dpi
proj=args.projection
xstep=args.xstep
ystep=args.ystep
# 
zzoom=args.ijwindow
zvp=args.lonlatwindow
# transform strings in integer and float
zoom=[ int(zzoom[0]), int(zzoom[1]), int(zzoom[2]), int(zzoom[3]) ]
vp=[ float(zvp[0]), float(zvp[1]), float(zvp[2]), float(zvp[3]) ]
#vp   = [ -100, -10, 40, 70 ]
#zoom = [2190, 1727,3900,3400]

print 

cf_plt=cf_in
cf_in=cf_in+".nc"

cdir_figs=args.figures
if not path.exists(cdir_figs): mkdir(cdir_figs)

print ' INPUT_FILE  : ', cf_in
print '    variable : ', cv_in
lmsk = False

# predifined variables ...
if cv_in == 'SST':
    cname='Surface temperature '
    vmin=14
    vmax=30
    offset=-273.
    scalef=1.
    unit='DegC'
    tick=2

elif cv_in == 'QFX':
    cname='Evaporation '
    vmin=0
    vmax=10
    offset=0
    scalef=86400.
    unit='mm/day'
    tick=1

elif cv_in == 'QFX_SEA':
    cname='Evaporation '
    vmin=0
    vmax=10
    offset=0
    scalef=86400.
    unit='mm/day'
    tick=1

elif cv_in == 'sos':
    cname='Sea Surface Salinity '
    vmin=35.5
    vmax=37.5
    offset=0
    scalef=1
    unit='--'
    tick=0.4
    lmsk = True
elif cv_in == 'sovitmod':
    cname='Sea Surface Velocity '
    vmin=0.
    vmax=1.
    offset=0
    scalef=1
    unit='m/s'
    tick=0.1
    lmsk = True
else:
    print 'ERROR : variable ',cv_in, ' not yet supported.' 
    quit()

print ' SCALE factor  OFFSET', scalef, offset, 'for ',cv_in

# set some data out of the time loop
vc_value = nmp.arange(vmin, vmax+0.1, tick) 

# Open the input file
id_in = Dataset(cf_in)
list_var = id_in.variables.keys()

Xlon = id_in.variables['nav_lon'][:,:]
Xlon =nmp.where(Xlon > 73, Xlon-360,Xlon )
Xlat = id_in.variables['nav_lat'][:,:]

(nj,ni) = nmp.shape(Xlon) ; print('Shape Arrays => ni,nj ='), ni,nj

# Prepare for dealing with the date of the fields
Xtim = id_in.variables['time_counter'][:]
(nt,)= nmp.shape(Xtim)

time_counter=id_in.variables['time_counter']
units=time_counter.getncattr('units')
cal='standard'
cal=time_counter.getncattr('calendar')
cdftime=nctime.utime(units,calendar=cal)

if frame == -1:
   frd=0
   fre=nt
else:
   if number == -1:
      frd=frame
      fre=frame+nt
   else:
      frd=frame
      fre=frame+number

for tim in range(frd,fre):
    V2d = scalef*(id_in.variables[cv_in][tim,:,:]+offset)
#    Vmsk=0.*V2d  + 1.e20
#    V2d=nmp.where(V2d == 0, Vmsk, V2d )
    dat=cdftime.num2date(Xtim[tim])
    datstr = dat.strftime("%b-%d-%Y %H:%M")
    if lmsk:
       V2d=nmp.ma.masked_where(V2d == 0 , V2d) 
    if tim < 10:
       cnum='00'+str(tim)
    elif tim < 100:   
       cnum='0'+str(tim)
    elif tim < 1000:   
       cnum=str(tim)

    print cnum, ' ', datstr

    vfig_size = [ 4, 4.5 ] 
    vsporg = [0.1, 0.12, 0.80, 0.75]
    eps=0.00  # 0.1
    ni=zoom[2]-zoom[0]+1
    nj=zoom[3]-zoom[1]+1
    print ni, nj

    lon_0= (vp[0]+vp[2])/2.
    lat_0= (vp[1]+vp[3])/2.
    lat_0=-90
    lon_0=150
    print "lon_0=", lon_0
    print "lat_0=", lat_0
#    vp   = [ -200, -89, 60, 89 ]
#    cfig = cdir_figs+'/'+cv_in+'_'+cnum+'_'+cmap+'.png'
    cfig = cdir_figs+'/'+cf_plt+'.png'
    
#    fig = plt.figure(num = 1, figsize=(vfig_size), dpi=None, facecolor='k', edgecolor='k')
    fig = plt.figure(num = 1,  dpi=None, facecolor='k', edgecolor='k')
#    fig = plt.figure(num = 1,  dpi=300, facecolor='k', edgecolor='k')
    ax  = plt.axes(vsporg, facecolor = 'w')
    
    carte = Basemap(llcrnrlon=vp[0]-eps, llcrnrlat=vp[1]-eps, urcrnrlon=vp[2]+eps, urcrnrlat=vp[3]+eps, \
                    resolution='c', area_thresh=10., projection=proj,boundinglat=-30, lon_0=lon_0,lat_0=lat_0,\
                    epsg=None)
    
    x0,y0 = carte(Xlon,Xlat)
    print type(x0)
    print x0[nj-1,ni-1]
    print y0[nj-1,ni-1]
#   x0=nmp.where(x0 > 73, x0-360,x0 )
    print zoom
    type (zoom[1])
    print x0[zoom[1]:zoom[3],zoom[0]:zoom[2]]
    
    nrm_value = colors.Normalize(vmin=vmin, vmax=vmax, clip=False)
    #cmap=['#0033FF','#0050FF','#006FFF','#008DFF','#15AAFF','#3BC8FF','#60E7FF','#91FFFF','#D7FFF5','#F5FFD7','#FFFF91','#FFE760','#FFC83B','#FFAA15','#FF8700','#FF5A00','#FF2D00']
#   ft = carte.pcolormesh(x0,y0,V2d, cmap = cmap, norm=nrm_value )
    ft = carte.pcolormesh(x0[zoom[1]:zoom[3],zoom[0]:zoom[2]],y0[zoom[1]:zoom[3],zoom[0]:zoom[2]],V2d[zoom[1]:zoom[3],zoom[0]:zoom[2]], cmap = cmap, norm=nrm_value )
#    ft = carte.pcolormesh(x0[1:nj-3,3:ni],y0[1:nj-3,3:ni],V2d[1:nj-3,3:ni], cmap = cmap, norm=nrm_value )
    #ft = carte.pcolormesh(x0,y0,V2d,  norm=nrm_value )
    #carte.etopo()
    #carte.shadedrelief()
    carte.drawcoastlines(linewidth=0.5)
    

#   xstep=45
#   ystep=20
    carte.drawmeridians(nmp.arange(vp[0],vp[2]+xstep,xstep), labels=[1,1,1,1], linewidth=0.3)
    carte.drawparallels(nmp.arange(vp[1],vp[3]+ystep,ystep), labels=[1,1,1,1], linewidth=0.3)

    # add color bar  : 
    ax3 = plt.axes( [0.1,0.05,0.80,0.015])

    clb = mpl.colorbar.ColorbarBase(ax3, ticks=vc_value, cmap=cmap, norm=nrm_value, orientation='horizontal')
    
    # Add title
#    ax.annotate('ENERGETICS', xy=(0.02, 0.9), xycoords='figure fraction')
    ax.annotate(cname+'('+unit+') '+datstr, xy=(0.3, 0.93),  xycoords='figure fraction')
    
    
    plt.savefig(cfig,dpi=dpi,orientation='portrait', transparent=False)
    plt.close(1)
