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
from charpal import *
from charlib import *
import math

##
warnings.filterwarnings("ignore")


# default values
misval  = -9999
mischr  = 'none'

#  ARGUMENT PARSING
parser = ap.ArgumentParser(description='Generate pixel maps of a given scalar.')
#  Required
requiredNamed = parser.add_argument_group('required arguments')
requiredNamed.add_argument('-i', '--input_file',         required=True,          help='specify the input file ...')
requiredNamed.add_argument('-v', '--variable'  ,         required=True,          help='specify the input variable')
requiredNamed.add_argument('-wij', '--ijwindow',         required=True, nargs=4, help=' data window in i,j coordinate (imin jmin imax jmax)' )
requiredNamed.add_argument('-wlonlat', '--lonlatwindow', required=True, nargs=4, help=' plot window in lonlat coordinate (lonmin latmin lonmax latmax)' )
#  Options
parser.add_argument('-p', '--palette',             default='plasma',required=False, help='specify the palette name')
parser.add_argument('-pc', '--chartpal',           default=mischr  ,required=False, help='specify the CHART-like palette name')
parser.add_argument('-lnam', '--long_name',        default=mischr  ,required=False, help='specify the long name of the variable')
parser.add_argument('-tit1', '--title1',           default=mischr  ,required=False, help='specify title-1 for the plot')
parser.add_argument('-tit2', '--title2',           default=mischr  ,required=False, help='specify title-2 for the plot')
parser.add_argument('-units', '--units',           default=mischr  ,required=False, help='specify unit names for plotted variable, instead of default (set in charlib)')
parser.add_argument('-o', '--output',              default=mischr  ,required=False, help='specify a root name for the plot. Numbering added automatically')
parser.add_argument('-d', '--figures',             default='./figs',required=False, help='specify the output directory')
parser.add_argument('-t', '--frame',  type=int,    default=-1,      required=False, help='specify the beg frame  number, [all]')
parser.add_argument('-nt', '--number',type=int,    default=-1,      required=False, help='specify the number of frame, [all]')
parser.add_argument('-dpi','--dpi',type=int,       default=150,     required=False, help='specify dpi resolution [150] ')
parser.add_argument('-klev','--klev',type=int,     default=-1,      required=False, help='specify vertical level to plot in 3D file ')
parser.add_argument('-proj','--projection',        default='cyl',   required=False, help='specify projection [cyl] ')
parser.add_argument('-xstep','--xstep',type=float, default=45.0,    required=False, help='specify longitude graduation [45] ')
parser.add_argument('-ystep','--ystep',type=float, default=45.0,    required=False, help='specify latitude graduation [45] ')
parser.add_argument('-vmin','--varmin',type=float, default=misval,  required=False, help='specify vmin for the variable ')
parser.add_argument('-vmax','--varmax',type=float, default=misval,  required=False, help='specify vmin for the variable ')
parser.add_argument('-clrlim','--clrlim',          default=mischr,  required=False, help='Give the name of a clrlim file')
parser.add_argument('-offset','--voff',type=float, default=misval,  required=False, help='specify offset valuoffset value ')
parser.add_argument('-scale','--vsca' ,type=float, default=misval,  required=False, help='specify scaling factor ')
parser.add_argument('-bckgrd','--bkgrd',           default=mischr,  required=False, help='specify a background map : [none], etopo, shadedrelief, bluemarble')
parser.add_argument('-figsz','--figsz', nargs=2,   default=[6.4,4.8],  required=False, help='specify figsize in inches ( width, height) ')
parser.add_argument('-res','--res',                default='c'   ,  required=False, help='specify the resolution of the coastline: one of c, l, i, h, f [c]' )
parser.add_argument('-depv','--depth_var',         default='deptht',required=False, help='specify the name of the depth variable in 3D file [deptht]' )
parser.add_argument('-dep','--dep'    ,type=float, default=-1.,     required=False, help='specify the dep where to plot (if klev specified, klev is taken) ')
parser.add_argument('-tick','--tick'  ,type=float, default=-1.,     required=False, help='indicate the tick spacing on the label bar' )
parser.add_argument('-log','--log'    ,action='store_true',         required=False, help='ask for a lo10 scale  ')
parser.add_argument('-orca','--orca'  ,action='store_true',         required=False, help='shift longitude > 73 degree for orca grid  ')

args = parser.parse_args()
####

# set variables according to command line arguments (or default values)
#required 
cf_in  = args.input_file
cv_in  = args.variable
zzoom  = args.ijwindow
zvp    = args.lonlatwindow
# optional
chpal  = args.chartpal
cmap   = args.palette
frame  = args.frame
number = args.number
dpi    = args.dpi
proj   = args.projection
xstep  = args.xstep
ystep  = args.ystep
varmin = args.varmin
varmax = args.varmax
clrlim = args.clrlim
voff   = args.voff
vsca   = args.vsca
bkgrd  = args.bkgrd
zfigsz = args.figsz
res    = args.res
klev   = args.klev
vdep   = args.depth_var
dep    = args.dep
clrlog = args.log
lorca  = args.orca
ticka  = args.tick
clname = args.long_name
tit1   = args.title1
tit2   = args.title2
cf_out = args.output
unita  = args.units

print tit1
print tit2


if klev == -1 and dep == -1:
   l3d = False
else:
   l3d = True

if chpal <> mischr:
   pal=Palette()
   dicopal=pal.__dict__
   ctmp=dicopal[chpal]
   cmap=colors.ListedColormap(ctmp)

# Read the color limit in a file. Each line contains a value from min to max. Each value will be labeled
# Read the color limit in a file. Each line contains a value from min to max. Each value will be labeled
if clrlim <> mischr:
   with open(clrlim) as f:
       vc_value=[float(line.rstrip()) for line in f]

   vmin=nmp.array(vc_value).min()
   vmax=nmp.array(vc_value).max()
   varmin=misval
   varmax=misval

# 
# transform strings in integer and float
#  zoom defines in model (i,j) the south-west  and north-east corners for the data 
#  define model zoom corners as imin, imax   jmin,jmax  for the sake or readibility
imin  = int(zzoom[0])  ; jmin = int(zzoom[1])
imax  = int(zzoom[2])  ; jmax = int(zzoom[3])

#  vp defines the view point  in geographical coordinates (lon,lat)  for south-west  and north-east corners of the plot
#    use lonmin,latmin lonmax,latmax for the sake of readybility
lonmin =  float(zvp[0])  ; latmin = float(zvp[1])
lonmax =  float(zvp[2])  ; latmax = float(zvp[3])

# use cf_in as the root name for plots and data
cf_plt = cf_in      # plot
cf_in  = cf_in+".nc" # data (extension .nc is assumed so far ! )

confcase = cf_in.split('_')[0]
tag      = cf_in.split('_')[1]
descrp   = cf_in.split('_')[2]

# define and create the output directory for the png files
cdir_figs = args.figures 
if not path.exists(cdir_figs): mkdir(cdir_figs)

print ' INPUT_FILE  : ', cf_in
print '    variable : ', cv_in
if l3d:
   print '    plot level :', klev
   print '    requ depth :', dep



# predifined variables ... for each predefined variables (according to their name in netcdf data file)
#                          some values are defined ... lmsk is set to true for ocean variable
#                          note the vmin and vmax can be overwritten using command line option -vmin <value> -vmax <value>
clrlayer=Color()   # Color define a class and its attribute.
                   # The idea is to follow CHART (NCL) principle with plot layer as object (color contour, vector
                   # overlay.
clrlayer.clrvar  = cv_in
clrlayer.clrdata = cf_in

colorinit(clrlayer) # use clrvar in order to identify some defaulta values
                    # This call replace the initialisation formely done in the program
                    # which was difficult to maintain and reduce readibility of the code

cname  = clrlayer.clrlnam

if clrlim == mischr:
   vmin   = clrlayer.clrmin
   vmax   = clrlayer.clrmax
else:
   clrlayer.clrmin = vmin
   clrlayer.clrmax = vmax

if clname <> mischr:
   cname = clname

offset = clrlayer.offset
scalef = clrlayer.scalef
unit   = clrlayer.unit

if  ticka == -1.:
  tick   = clrlayer.tick
else:
  tick   = ticka
  clrlayer.tick = tick

lmsk   = clrlayer.lmsk

# scale factor and offset are intended for units changes  (for instance from K to degC, or from kg/m2/s to mm/day ... )
print ' SCALE factor  OFFSET', scalef, offset, 'for ',cv_in

# update  the values passed as options
if varmin != misval:
   vmin = varmin

if varmax != misval:
   vmax = varmax

if voff != misval:
   offset = voff

if vsca != misval:
   scalef = vsca

# set some data out of the time loop
if clrlim == mischr:
   vc_value = nmp.arange(vmin, vmax+0.1, tick) 

# update units with argument
if unita != mischr:
   unit = unita

# Open the input file
id_in = Dataset(cf_in)
id_in.set_auto_mask(False)
list_var = id_in.variables.keys()

Xlon = id_in.variables['nav_lon'][:,:]
#  This point is to be improved... It works dor PAcific values in order to avoid discontinuity in longitude at the date line (180 E/W) 
### JMM 73
if lorca :
   Xlon =nmp.where(Xlon > 73, Xlon-360,Xlon )
   #Xlon =nmp.where(Xlon < 0 , Xlon+360,Xlon )

Xlat = id_in.variables['nav_lat'][:,:]

# read depth in case of 3D file and 3D var to plot
if l3d:
   gdep   = id_in.variables[vdep][:]
   (npk,) = nmp.shape(gdep)
   if dep <> -1 :
      for k in range(0,npk):
          if gdep[k] < dep :
             k0=k
     
      alpha=( float(dep) - gdep[k0] ) / (gdep[k0+1] - gdep[k0] )
      deplabel = int(round(dep))
      print "    depth : "+str(dep)+"m"
      print "    between : "+str(k0)+" and "+str(k0+1)
   else:
      print "    depth : "+str(gdep[klev])+"m"
      print "    klev  : "+str(klev)
      deplabel = int(round(gdep[klev]))

# get the size of the data set from the nav_lon variable (why not ? ) 
(npjglo,npiglo) = nmp.shape(Xlon) ; print('Shape Arrays => npiglo,npjglo ='), npiglo,npjglo

# Prepare for dealing with the date of the fields
Xtim = id_in.variables['time_counter'][:]
(nt,)= nmp.shape(Xtim)

time_counter=id_in.variables['time_counter']
units=time_counter.getncattr('units')
cal='standard'
try:
  cal=time_counter.getncattr('calendar')
except:
  cal='standard'
  units="days since 1955-01-01 00:00:00"

cdftime=nctime.utime(units,calendar=cal)

# time frame selection
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

miss=0.
try:
   miss=id_in.variables[cv_in].getncattr('_FillValue')
except:
   pass

try:
   miss=id_in.variables[cv_in].getncattr('missing_value')
except:
   pass
print ("use missing/Fill value : ", miss)

for tim in range(frd,fre):
    # read the data and apply scaling and offset (can be 1 and 0 btw ... )

    if l3d :
       if klev == -1 :
          V2d0 = scalef*(id_in.variables[cv_in][tim,k0,  :,:]+offset)
          V2d1 = scalef*(id_in.variables[cv_in][tim,k0+1,:,:]+offset)
          V2d  = alpha * V2d1 + (1. - alpha) * V2d0
          V2d  = nmp.ma.masked_where(V2d1 == miss, V2d )
       else :
          V2d = scalef*(id_in.variables[cv_in][tim,klev,:,:]+offset)
    else:
       V2d = scalef*(id_in.variables[cv_in][tim,:,:]+offset)

    dat=cdftime.num2date(Xtim[tim])
    datstr = dat.strftime("%b-%d-%Y %H:%M")
#  masking is moved later on leave comment here for record 
#    if lmsk:
#       V2d=nmp.ma.masked_where(V2d == 0 , V2d) 

#  Frame numering on 4 digits (allowing 9999 time frames)
    if tim < 10:
       cnum='00'+str(tim)
    elif tim < 100:   
       cnum='0'+str(tim)
    elif tim < 1000:   
       cnum=str(tim)

    print cnum, ' ', datstr

    # define size of the data zoom
    nj=jmax - jmin + 1  

    if  imax > imin :   # standard case
       ni=imax - imin + 1
    else:               # crossing the periodic line (73E in ORCA grids) ...
       ni=imax - imin + npiglo -1   # take care of overlap of 2 points at periodicity

    print "Zoom data size: ", ni,"x", nj

# Not clear : crappy hard coded  stuff
    #vfig_size = [ 4, 4.5 ] 
    vfig_size = [ float(zfigsz[0]), float(zfigsz[1]) ]
    fc='w'
    if proj <> "noproj":
        #        left bot   width  height
        vsporg = [0.1, 0.12, 0.80, 0.75]
    else:
        fc='lavender'  # force missing value to be shaded by fc
        if ni >= nj :
            height=(0.75*nj)/ni
            vsporg = [0.1,0.15,0.8,height ]
        else  :  # ni < nj
            width = (0.8*ni)/nj
            vsporg = [0.5-width/2.,0.15,width, 0.75 ]
    eps=0.10  # 0.1


# set variables for some projections.. To be improved for more projection (cyl and merc OK so far)
    lon_0 = (lonmin + lonmax)/2.
    lat_0 = (latmin + latmax)/2.
    lat_0=0   # ugly fix : force lat_0 to be 0 ... 
    print "Longitude of the center of the plot lon_0 =", lon_0
    print "Latitude  of the center of the plot lat_0 =", lat_0

# full path for the output filename... Time frame numbering broken 
#   need to restore something for multi time frame files ...
    if cf_out == mischr:
      cfig = cdir_figs+'/'+cf_plt+'_'+cnum+'.png'
    else:
      cfig = cdir_figs+'/'+cf_out+'_'+cnum+'.png'
    
# Defining the map with matplotlib/basemap : Inspired from Laurent's code (kind of black box for JM)
#    fig = plt.figure(num = 1, figsize=(vfig_size), dpi=None, facecolor='k', edgecolor='k')
#    fig = plt.figure(num = 1,  dpi=None, facecolor='k', edgecolor='k')
    fig = plt.figure(num = 1,  figsize=(vfig_size), dpi=None, facecolor='k', edgecolor='k')
    ax  = plt.axes(vsporg, facecolor = fc)
    
    if proj == "merc"  or proj == "cyl":
        carte = Basemap(llcrnrlon=lonmin-eps, llcrnrlat=max(latmin-eps,-90), urcrnrlon=lonmax+eps, urcrnrlat=min(latmax+eps,90), \
                    resolution=res, area_thresh=10., projection=proj, lon_0=lon_0, lat_0=lat_0, \
                    epsg=None)
    elif proj == "noproj":
        carte = ax
        x0=nmp.array([range(1,npiglo+1) for j in range (npjglo)])
        y0=nmp.array([[j]*npiglo for j in range(1,npjglo+1)])
    else:
        print "projection "+proj+"  is not suppoted yet :("
        quit()
    
    if proj <> "noproj":
        x0,y0 = carte(Xlon,Xlat)

#
    if  imin > imax :  # need to shuffle the data and lon lat ...
#      initialize plotting array (pxxx) with zeros. So far create the array at the right size (zoom data)
       print "  Proceed to data shift for crossing the periodic line..."
       pV2d  = nmp.zeros((nj,ni))
       px0   = nmp.zeros((nj,ni))
       py0   = nmp.zeros((nj,ni))
       ndim=len(V2d.shape)
#      for field value V2d (to be plotted) and for x-coord (x0) and y-coord (y0) variables
#      fill the data according to the periodic line and overlap
       if ndim == 2:
         pV2d[0:nj,            0:npiglo-imin-1]  = V2d[jmin:jmax+1,imin:npiglo-1]
         pV2d[0:nj,npiglo-imin-1:ni           ]  = V2d[jmin:jmax+1,   1:imax+1  ]
       elif ndim == 3:
         pV2d[0:nj,            0:npiglo-imin-1]  = V2d[0,jmin:jmax+1,imin:npiglo-1]
         pV2d[0:nj,npiglo-imin-1:ni           ]  = V2d[0,jmin:jmax+1,   1:imax+1  ]

       px0[0:nj,            0:npiglo-imin-1]   = x0[jmin:jmax+1,imin:npiglo-1]
       px0[0:nj,npiglo-imin-1:ni           ]   = x0[jmin:jmax+1,   1:imax+1  ]

       py0[0:nj,            0:npiglo-imin-1]   = y0[jmin:jmax+1,imin:npiglo-1]
       py0[0:nj,npiglo-imin-1:ni           ]   = y0[jmin:jmax+1,   1:imax+1  ]


# Some debugging print  accross the periodic line
       print px0[0,npiglo-imin -5:npiglo-imin+5] 
       print "======================"
       print py0[0,npiglo-imin -5:npiglo-imin+5]
       print "======================"
       print pV2d[0,npiglo-imin -5:npiglo-imin+5]
       if lmsk:
          pV2d=nmp.ma.masked_where(pV2d == 1    , pV2d)   # this 1 value is strange ...
          pV2d=nmp.ma.masked_where(pV2d == miss , pV2d) 
          pV2d=nmp.ma.masked_where(pV2d == 0    , pV2d) 
    else:
#      Just extract the zoom
       pV2d = V2d[jmin:jmax , imin:imax ]
       px0  =  x0[jmin:jmax , imin:imax ]
       py0  =  y0[jmin:jmax , imin:imax ]
#   Apply masking if required by the variable (assume here that 0  is the _FillValue or missing_value on land)
       if lmsk:
          pV2d=nmp.ma.masked_where(pV2d == miss , pV2d) 
          pV2d=nmp.ma.masked_where(pV2d == 0    , pV2d) 

    if clrlog :
#      pV2d=nmp.log10(pV2d)
#      nrm_value = colors.Normalize(vmin=math.log10(vmin),  vmax=math.log10(vmax), clip=False)
      nrm_value = colors.LogNorm(vmin=vmin,  vmax=vmax, clip=False)
    else:
      nrm_value = colors.Normalize(vmin=vmin, vmax=vmax, clip=False)

# Keep track of various tests ...
    #cmap=['#0033FF','#0050FF','#006FFF','#008DFF','#15AAFF','#3BC8FF','#60E7FF','#91FFFF','#D7FFF5','#F5FFD7','#FFFF91','#FFE760','#FFC83B','#FFAA15','#FF8700','#FF5A00','#FF2D00']
    ft = carte.pcolormesh(px0,py0,pV2d, cmap = cmap, norm=nrm_value )

    if proj <> "noproj":
            #   comment nice features for land filling as it uses lot of memory (ORCA12 case.)
            #   Background option : default is 'none' : do nothing
            #   possible method includes : bluemarble, shadedrelief, etopo
            if bkgrd == 'etopo':
               carte.etopo()

            if bkgrd == 'shadedrelief':
               carte.shadedrelief()

            if bkgrd == 'bluemarble' :
               carte.bluemarble()

            carte.drawcoastlines(linewidth=0.5)

            # if longitude 0 is in the longitude range, force longitude 0 to be labeled
            if lonmin <=  0  and lonmax >= 0:
               nmeridian= int((lonmax -lonmin ) /xstep)
               meridians= nmp.arange(-nmeridian * xstep, nmeridian *xstep, xstep )
            else:
               meridians=nmp.arange(lonmin,lonmax+xstep,xstep)
            # if  Equator is in the latitude range, force Equator to be labeled
            if latmin <=  0  and latmax >= 0:
               nparallel = int((latmax -latmin ) /ystep)
               parallels = nmp.arange(-nparallel * ystep, nparallel *ystep, ystep )
            else:
               parallels=nmp.arange(latmin,latmax+ystep,ystep)

            # may be usefull to choose labels on options, using those actuals values as default
            carte.drawmeridians(meridians, labels=[1,0,0,1], linewidth=0.3)
            carte.drawparallels(parallels, labels=[1,0,0,1], linewidth=0.3)

    # add color bar  : Crappy numbers linked to vsporg
    ax3 = plt.axes( [0.1,0.05,0.80,0.015])

    if clrlog :
        formatter = mpl.ticker.LogFormatter(base=10, labelOnlyBase=False,minor_thresholds=(1000,1000) )
        print vc_value, vmin, vmax
        clb = mpl.colorbar.ColorbarBase(ax3, ticks=vc_value, cmap=cmap, norm=nrm_value, orientation='horizontal',format=formatter)
#        clb = mpl.colorbar.ColorbarBase(ax3, ticks=[1,10,100,1000,5000], cmap=cmap, norm=nrm_value, orientation='horizontal',format=formatter)
    else:
        clb = mpl.colorbar.ColorbarBase(ax3, ticks=vc_value, cmap=cmap, norm=nrm_value, orientation='horizontal')
    
    # Add title
    if tit2 == mischr :
       if l3d:
          ax.annotate(str(deplabel)+"m "+cname+'('+unit+') '+datstr, xy=(0.3, 0.91),  xycoords='figure fraction')
       else:
          ax.annotate(cname+'('+unit+') '+datstr, xy=(0.3, 0.91),  xycoords='figure fraction')
    else:
        ax.annotate(tit2, xy=(0.3,0.91), xycoords='figure fraction')

    if tit1 == mischr :
        ax.annotate(confcase, xy=(0.3, 0.94),  xycoords='figure fraction')
    else:
        ax.annotate(tit1, xy=(0.3, 0.94),  xycoords='figure fraction')
    
    # save plot to file
    plt.savefig(cfig,dpi=dpi,orientation='portrait', transparent=False)
    plt.close(1)
