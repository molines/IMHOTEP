from netCDF4 import Dataset
from argparse import ArgumentParser
import numpy as np
import sys

#
# Basic iceberg trajectory post-processing python script.
# This script collates iceberg trajectories from the distributed datasets written
# out by each processing region and rearranges the ragged arrays into contiguous
# streams for each unique iceberg. The output arrays are 2D (ntraj, ntimes) arrays.
# Note that some icebergs may only exist for a subset of the possible times. In these
# cases the missing instances are filled with invalid (NaN) values.
#
# Version 2.0 August 2017. Adapted to process all variables and retain original
#                          datatypes. (acc@noc.ac.uk)

parser = ArgumentParser(description='produce collated trajectory file \
                                     from distributed output files, e.g. \
                                     \n python ./icb_pp.py \
                                     -t  trajectory_icebergs_004248_ \
                                     -n 296 -o trajsout.nc' )

parser.add_argument('-t',dest='froot',
                         help='fileroot_of_distrbuted_data; root name \
                               of  distributed trajectory output (usually \
                               completed with XXXX.nc, where  XXXX is the \
                               4 digit processor number)', 
                      default='trajectory_icebergs_004248_')

parser.add_argument('-n',dest='fnum',help='number of distributed files to process', 
                         type=int, default=None)

parser.add_argument('-o',dest='fout',
                         help='collated_output_file; file name to receive \
                              the collated trajectory data', default='trajsout.nc')

args = parser.parse_args()

default_used = 0
if args.froot is None:
    pathstart = 'trajectory_icebergs_004248_'
    default_used = 1
else:
    pathstart = args.froot

if args.fnum is None:
    procnum = 0
    default_used = 1
else:
    procnum = args.fnum

if args.fout is None:
    pathout = 'trajsout.nc'
    default_used = 1
else:
    pathout = args.fout

if default_used == 1:
   print('At least one default value will be used; command executing is:')
   print('icb_pp.py -t ',pathstart,' -n ',procnum,' -o ',pathout)

if procnum < 1:
   print('Need some files to collate! procnum = ',procnum)
   sys.exit(11)

icu = []
times = []
#
# Loop through all distributed datasets to obtain the complete list
# of iceberg identification numbers and timesteps
#
for n in range(procnum):
    nn = '%4.4d' % n
    fw = Dataset(pathstart+nn+'.nc')
    # keep a list of the variables in the first dataset
    if n == 0:
        varlist = fw.variables
    #
    # skip any files with no icebergs
    if len(fw.dimensions['n']) > 0:
        print pathstart+nn+'.nc'
        ic = fw.variables['iceberg_number'][:,0]
        ts = fw.variables['timestep'][:]
        icv = np.unique(ic)
        ts = np.unique(ts)
        print('Min Max ts: ',ts.min(), ts.max())
        print('Number unique icebergs= ',icv.shape[0])
        icu.append(icv)
        times.append(ts)
    fw.close()
#
# Now flatten the lists and reduce to the unique spanning set
#
try:
    icu = np.concatenate(icu)
except ValueError:
    # No icebergs: create an empty output file.
    print 'No icebergs in the model.'
    fw = Dataset(pathstart+'0000.nc')
    fo = Dataset(pathout, 'w', format='NETCDF4_CLASSIC')
    ntrj = fo.createDimension('ntraj', None)
    icbn = fo.createVariable('iceberg_number', 'i4',('ntraj'))
    n = 0
    for key, value in varlist.iteritems() :
        if key != "iceberg_number" :
            print 'key is ',key
            oout = fo.createVariable(key, value.dtype, ('ntraj'),
                                 zlib=True, complevel=1)
            oout.long_name = fw.variables[key].getncattr('long_name')
            oout.units = fw.variables[key].getncattr('units')
            n = n + 1
    fw.close()
    fo.close()    
    sys.exit()

icu = np.unique(icu)
times = np.concatenate(times)
times = np.unique(times)
ntraj = icu.shape[0]
print(ntraj, ' unique icebergs found across all datasets')
print('Icebergs ids range from: ',icu.min(), 'to: ',icu.max())
print('times range from:        ',times.min(), 'to: ', times.max())
#
# Declare array to receive data from all files
#
nt = times.shape[0]
#
n=0
for key, value in varlist.iteritems() :
    if key != "iceberg_number" :
        n = n + 1
inarr = np.zeros((n, ntraj, nt))
#
# initially fill with invalid data
#
inarr.fill(np.nan)
#
# Declare some lists to store variable names, types and long_name and units attributes
# iceberg_number gets special treatment
innam = []
intyp = []
inlngnam = []
inunits = []
for key, value in varlist.iteritems() :
    if key != "iceberg_number" :
        innam.append(key)
#
# reopen the first datset to collect variable attributes
# (long_name and units only)
#
nn = '%4.4d' % 0
fw = Dataset(pathstart+nn+'.nc')
for key, value in varlist.iteritems() :
    if key != "iceberg_number" :
        intyp.append(fw.variables[key].dtype)
        inlngnam.append(fw.variables[key].getncattr('long_name'))
        inunits.append(fw.variables[key].getncattr('units'))
fw.close()
#
# loop through distributed datasets again, this time
# checking indices against icu and times lists and
# inserting data into the correct locations in the 
# collated sets.
#
for n in range(procnum):
    nn = '%4.4d' % n
    fw = Dataset(pathstart+nn+'.nc')
#
# Note many distributed datafiles will contain no iceberg data
# so skip quickly over these
    m  = len(fw.dimensions['n'])
    if m > 0:
        inx = np.zeros(m, dtype=int)
        tsx = np.zeros(m, dtype=int)
        #print pathstart+nn+'.nc'
        ic = fw.variables['iceberg_number'][:,0]
        ts = fw.variables['timestep'][:]
        for k in range(m):
            inxx   = np.where(icu == ic[k])
            inx[k] = inxx[0]
        for k in range(m):
            inxx   = np.where(times == ts[k])
            tsx[k] = inxx[0]
        n = 0
        for key, value in varlist.iteritems() :
            if key != "iceberg_number" :
                insmall = fw.variables[innam[n]][:]
                inarr[n,inx[:],tsx[:]] = insmall[:]
                n = n + 1
    fw.close()
#
# Finally create the output file and write out the collated sets
#
fo = Dataset(pathout, 'w', format='NETCDF4_CLASSIC')
ntrj = fo.createDimension('ntraj', ntraj)
nti  = fo.createDimension('ntime', None)
icbn = fo.createVariable('iceberg_number', 'i4',('ntraj'))
icbn[:] = icu
n = 0
for key, value in varlist.iteritems() :
    if key != "iceberg_number" :
        oout = fo.createVariable(innam[n], intyp[n], ('ntraj','ntime'),
                                 zlib=True, complevel=1, chunksizes=(1,nt))
        oout[:,:] = inarr[n,:,:]
        oout.long_name = inlngnam[n]
        oout.units = inunits[n]
        n = n + 1
fo.close()
