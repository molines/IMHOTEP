#!/usr/bin/env python
"""
   This program is used to build NEMO runoff file from gridded ISBA global file giving
   the river discharge along the coast line.
   This program is directly derived from J. Jouanno tool, used for the same purpose
   but at regional scale.
   Need to perform conda activate BASEMAP on JZ
"""

from netCDF4 import Dataset as netcdf
import datetime
import numpy as np
import shutil
import matplotlib.pyplot as plt
from numpy.ma import masked
from datetime import datetime, timedelta 
from netCDF4 import num2date, date2num 
import sys 

def bissextile(annee):
    return (annee%4==0 and annee%100!=0) or annee%400==0

INDIR='./'
OUTDIR='./'
RIVDIR='./'
radius=2.5   # formely 0.75

### Read river runoff ISNA ISBA
filein=RIVDIR+'sfxcm6_05d_erai_gpcc_daily_rivdis_mouth_1979-2019.nc'
ncisba = netcdf(filein,'r');
param_discharge='rivdis'
param_time='time_counter'
lon=np.array(ncisba.variables['longitude'][:])
lat=np.array(ncisba.variables['latitude'][:])
nj_isba=len(lat)
ni_isba=len(lon)
mask_isba=np.mean(ncisba.variables[param_discharge][0:365,:,:],0)
rivleft=mask_isba[:,:].copy()

### Read NEMO mesh 
filein2=INDIR+'mesh_mask.nc'
ncfile = netcdf(filein2,'r');
nav_lon=np.array(ncfile.variables['nav_lon'][:,:])
nav_lat=np.array(ncfile.variables['nav_lat'][:,:])
tmask=np.array(ncfile.variables['tmask'][0,:,:,:])
e1t=np.array(ncfile.variables['e1t'][0,:,:])
e2t=np.array(ncfile.variables['e2t'][0,:,:])
ncfile.close()
nz,nj,ni=tmask.shape

### Bande cotiere mask_nemo
coast_nemo=np.zeros((nj,ni))
for i in range(1,ni-1):
  for j in range(1,nj-1):
      if (4 > (tmask[0,j, i+1] + tmask[0,j, i-1] + tmask[0,j-1, i] +tmask[0, j+1, i]) > 0) & tmask[0, j, i]!=0:
          coast_nemo[j, i]=1
      else:
          coast_nemo[j, i]=0

## Limites domaine 
lonmin=np.min(nav_lon)
lonmax=np.max(nav_lon)
latmin=np.min(nav_lat)
latmax=np.max(nav_lat)


## Let's go !
idaystart=0  ## si on demarre au 1er janv 1979 
for year in range(1979,2020):
  #
  if bissextile(year): 
     ntyear=366
  else:
     ntyear=365
  #
  runoff_nemo=np.zeros((ntyear,nj,ni))
  mask_littoral=coast_nemo[:,:].copy()
  mask_littoral=np.where(mask_littoral==0,np.NaN,1) 
  total_taken=0
  total_missed=0
  #
  for i in range(ni_isba):
     print (i )
     for j in range(nj_isba):
       #
       # Special case for special rivers...
       #
#       if (i,j) in [(237,199),(261,179),(262,177),(181,238)]:
       if (i,j) in [(1000,1000)]:   # likely never happens in ISBA !
        if (i,j)==(237,199): 
          # Orenoque
          npoints=2
          list_points=[(153,180),(153,181)]
        elif (i,j)==(261,179):
          # Amazone
          npoints=3
          list_points=[(195,148),(195,147),(198,146)]
        elif (i,j)==(262,177):
          # Para 
          npoints=2
          list_points=[(202,141),(203,141)]
        elif (i,j)==(181,238):
          # Mississipi 
          npoints=2
          list_points=[(40,268),(37,267)]
        debit=1000*ncisba.variables[param_discharge][idaystart:idaystart+ntyear,j,i]
        for nb_point in range(npoints): ## on cherche ici 1 point
           for (indx,indy) in list_points:
                 if tmask[0,indy,indx]==0:
                     print("===Oh putain=====")
                     print(" ===> ",indx," ",indy) 
                     sys.exit() 
                 runoff_nemo[:,indy,indx]=debit[:]/ (npoints * e1t[indy,indx]*e2t[indy,indx] )
                 mask_littoral[indy,indx]=np.NaN
        rivleft[j,i]=0
       #
       # Common case for the remaining rivers
       #
       else:
           lon_isba=lon[i]
           lat_isba=lat[j]
           # On sort de la boucle si :
           # - le point n'est pas un point riviere 
           # - on est hors du domaine modele 
           if ( (lon_isba > lonmax) | (lon_isba < lonmin) ) : continue 
           if ( (lat_isba > latmax) | (lat_isba < latmin) ) : continue
           if mask_isba[j,i] is masked : continue          
           # Trouver l'indice TROP025 le plus proche d une donnee de debit et contenu dans le masque littoral            
           # 
           if mask_isba[j,i] < 2000: 
               npoints=1
           else:
               npoints=3
           #
           debit=1000*ncisba.variables[param_discharge][idaystart:idaystart+ntyear,j,i]               
           for nb_point in range(npoints): ## on cherche ici 1 point
                 dist=(np.power(nav_lon-lon_isba,2) + np.power(nav_lat-lat_isba,2) )*mask_littoral[:,:]
                 #Garder les points a une distance max de radius deglon/lat 
                 mindist=np.nanmin(np.sqrt(dist))
                 if mindist > radius: 
                     print("====>  Trop loin : ISBA lon=",lon_isba, " et lat=",lat_isba)
                     print("        mindist = ", mindist, " deg", nb_point, mask_isba[j,i] )
                     total_missed=total_missed+mask_isba[j,i]
                     continue 
                 rivleft[j,i]=1.e20  # isba runoff will be used
                 indy_arr, indx_arr=np.where(dist == np.nanmin(dist))          
                 ## test length array indx|y
                 if len(indy_arr) > 1:
                     indy=np.array([indy_arr[0]])
                     indx=np.array([indx_arr[0]])
                 else:
                     indy=indy_arr
                     indx=indx_arr
                 ## fill
                 runoff_nemo[:,indy,indx]=debit[:,np.newaxis]/ (npoints * e1t[indy,indx]*e2t[indy,indx] )
                 total_taken=total_taken+mask_isba[j,i]
                 mask_littoral[indy,indx]=np.NaN               
  
  # socoefr0
  socoefr0_nemo=np.where(runoff_nemo!=0,0.5,0)

  # Ecriture fichier
  fileout=OUTDIR+'eORCA025_runoff_ISBA_'+str(year)+'.nc'
  ncfileout=netcdf(fileout,'w')
  ncfileout.createDimension('x',ni)
  ncfileout.createDimension('y',nj)
  ncfileout.createDimension('time_counter',None)
  netcdf_time_counter=ncfileout.createVariable('time_counter','f',('time_counter')) # idem .createVariable('time',np.float64,(tdim.name,))
  netcdf_lon=ncfileout.createVariable('nav_lon','f',('y','x'))
  netcdf_lat=ncfileout.createVariable('nav_lat','f',('y','x'))
  netcdf_sorunoff=ncfileout.createVariable('sorunoff','f',('time_counter','y','x'))
  netcdf_socoefr0=ncfileout.createVariable('socoefr','f',('y','x'))
  ### on recupere les dates du fichier ISBA
  netcdf_sorunoff.units="kg/m2/s"
  netcdf_time_counter.units=ncisba.variables[param_time].units
  netcdf_time_counter[:]=ncisba.variables[param_time][idaystart:idaystart+ntyear]
  netcdf_lon[:,:]=nav_lon
  netcdf_lat[:,:]=nav_lat
  netcdf_sorunoff[:,:,:]=runoff_nemo
  netcdf_socoefr0[:,:]=socoefr0_nemo[0,:,:]
  ncfileout.close()
  # Write control file :  the ISBA values not distributed...
  fileout=OUTDIR+'not_used_ISBA.nc'
  ncfileout=netcdf(fileout,'w')
  ncfileout.createDimension('longitude',ni_isba)
  ncfileout.createDimension('latitude',nj_isba)
  netcdf_lon=ncfileout.createVariable('longitude','f',('longitude'))
  netcdf_lat=ncfileout.createVariable('latitude','f',('latitude'))
  netcdf_left=ncfileout.createVariable('rivdisleft','f',('latitude','longitude'),fill_value=1.e20)
  netcdf_left.missing_value=1.e20
#
  netcdf_lon[:]=lon
  netcdf_lat[:]=lat

  netcdf_left[:,:]=rivleft
  ncfileout.close()

  # next year
  idaystart+=ntyear
  print year
  print "Total river discharge taken : ", total_taken, "m3/s annual mean"
  print "Total river discharge missed : ", total_missed, "m3/s annual mean"

