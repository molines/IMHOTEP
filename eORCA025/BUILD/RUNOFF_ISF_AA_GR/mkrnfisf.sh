#!/bin/bash

CONFIG=eORCA025.L75
CONFIG2D=${CONFIG%.*}

OPENDAP=https://ige-meom-opendap.univ-grenoble-alpes.fr/thredds/fileServer/meomopendap/extract/eORCA025.L121/eORCA025.L121-BLD/ISF/eORCA025_mskisf_b0.2_c3.0_d1.0_v0.0
MSK_ISF=${CONFIG2D}_mskisf_b0.2_c3.0_d1.0_v0.0.nc
RIGNOT=${CONFIG2D}_mskisf_c3.0_v0.0.txt

MESH_MASK=$WORK/${CONFIG}/${CONFIG}-I/${CONFIG}_mesh_mask_closed_seas_greenland.nc

# output file name
RNF_ISF=${CONFIG2D}_rnfisf_b0.2_c3.0_d1.0_v0.0.nc
#-----------------------------------------------------------
cd $WORK/${CONFIG}/${CONFIG}-I/RUNOFF_ANTARCTIC-build

ln -sf $MESH_MASK mask.nc
ln -sf $MESH_MASK mesh_hgr.nc
ln -sf $MESH_MASK mesh_zgr.nc

ln -sf $DEVGIT/CDFTOOLS/bin/cdfisf_rnf ./

# get file if necessary
if [ ! -f $RIGNOT ] ; then
   wget $OPENDAP/$RIGNOT
fi

if [ ! -f $MSK_ISF ] ; then
   wget $OPENDAP/$MSK_ISF
fi

# wrapper for cdfisf_rnf : create RNF_ISF 
./cdfisf_rnf  -w 1 -f $MSK_ISF -v mask_isf \
                   -l $RIGNOT \
                   -b mask.nc  -vb tmaskutil \
                   -i $MSK_ISF -vi mask_isf -nc4 \
                   -o $RNF_ISF

exit
  usage : cdfisf_rnf -f ISF-fill-file -v ISF-fill_var -l ISF-listfile -w width 
      [-b BATHY-file] [-vb BATHY-var] [-i ISFDRAFT-file] [-vi ISFDRAFT-variable]
      [-nc4] [-o OUT-file ]
       
      PURPOSE :
         Build a netcdf file runoff file using the basal melting of the 
         ice-shelves. This netcdf file is intented to be used with NEMO when
         nn_isf namelist parameter is set to 3.
       
      ARGUMENTS :
           -f ISF-fill_file : file built by cdffill (all the ice shelves are
                              tagged with an id)
           -v ISF-fill_var  : name of fill variable to use in ISF-fill_file
           -l ISF-list : Text file with the melting rate (GT/y) given for
                each ice shelf.
           -w width : specify the width (in grid points) on which the run-off
                will be applied.
       
      OPTIONS :
           -b BATHY-file : give name of bathy file.
                       [ default : bathy.nc ]
           -vp BATHY-var : give name of bathy variable.
                       [ default : Bathymetry ]
           -i ISFDRAFT-file : give name of isf_draft file.
                       [ default : isf_draft.nc ]
           -vi ISFDRAFT-var : give name of isf_draft variable.
                       [ default : isf_draft ]
           -nc4 : Use this option to have netcdf4 output file, with chunking
                and deflation.
           -o OUT-file : Specify the name of the output file instead of 
                the default name rnfisf.nc
       
      REQUIRED FILES :
        mesh_hgr.nc and all files specified on the command line
       
      OUTPUT : 
        netcdf file : rnfisf.nc unless -o option used
          variables : sozisfmax (m), sozisfmin(m), sofwfisf (kg/m2/s)
       
      SEE ALSO :
        cdfisf_fill, cdfisf_forcing, cdfisf_poolchk
