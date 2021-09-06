#!/bin/bash

# This script create 2 files  corresponding to GI run ( Amazona Orinoco, Niger Congo  climatological)
#                                       and to GA run ( Gange Bramapoutre Irrawadi climatological)

# Climatological ISBA file :
ISBA_CLIM=../eORCA025_runoff_ISBA_noAA_noGR_clim_366.nc

############
#  GI 
###########
# Create mask used to keep only specific Atlantic rivers.
# Amazona Orinoco
AO=" 881 1021  651 741"
cdfmkmask -s $ISBA_CLIM -var sorunoff -zoomij $AO -o AO.mask.nc
# Niger Congo
NC=" 1156 1215 652 716"
cdfmkmask -s $ISBA_CLIM -var sorunoff -zoomij $NC -o NC.mask.nc

# add the 2 masks for GI mask
ncbo -O -4 -L 1 -y add -v tmask AO.mask.nc NC.mask.nc -o GI.mask.nc

############
#  GA
############
# Gange Bramapoutre
GB="49 96 761 791" 
cdfmkmask -s $ISBA_CLIM -var sorunoff -zoomij $GB -o GB.mask.nc
# Irrawady
IW="78 105 740 761"
cdfmkmask -s $ISBA_CLIM -var sorunoff -zoomij $IW -o IW.mask.nc

# add the 2 masks for GA mask
ncbo -O -4 -L 1 -y add -v tmask GB.mask.nc IW.mask.nc -o GA.mask.nc


## Create data file for GI and GA
ln -sf $ISBA_CLIM tmp.nc

cdfmltmask -f tmp.nc -m GI.mask.nc -v sorunoff -p T -M tmask -o GI_runoff_ISBA_noAA_noGR_clim_366.nc -noup
cdfmltmask -f tmp.nc -m GA.mask.nc -v sorunoff -p T -M tmask -o GA_runoff_ISBA_noAA_noGR_clim_366.nc -noup
exit
  usage : cdfmkmask -s S-file [-zoom lonmin lonmax latmin latmax] ...
                    ... [-var  VAR-name ] ...
                    ... [-zoomij iimin iimax ijmin ijmax] ...
                    ... [-zoombat bathymin bathymax]  ...
                    ... [-zoomvar varname varmin varmax]  ...
                    ... [-fill iiseed jjseed] ...
                    ... [-bfij BOUND_IJ-file.txt] ...
                    ... [-bflonlat BOUND_LONLAT-file.txt] ...
                    ... [-time ] [-o OUT-file]
       
      PURPOSE :
        In its simplest use, this tool builds a mask file from the salinity 
        field read from the input file, assuming that land points are set to
        _Fill_Value.
        
        A growing set of options, extend this tool to a much more sophisticated
        mask builder: use of other variables than salinity, range criteria on
        the variables, geographical or model limits, and the possibility of
        using any combination of the criteria.
        
       
      ARGUMENTS :
        -s S-file : netcdf file with salinity.
                * if S-file = -maskfile, we assume a reference file named 
 mask.nc
                    with tmask variable.
                * if S-file = -2dmaskfile, we assume a reference file named 
                   mask.nc with tmaskutil variable.
                * if S-file = -mbathy, we assume a reference file named 
                    bathylevel.nc with mbathy variable, giving the number of 
                    levels in the ocean.
       
      OPTIONS :
        [-var  VAR-name ] : give the name of the variable to work with, instead
                          vosaline.
        [-zoom lonmin lonmax latmin latmax] : geographical windows used to
                         limit the area where the mask is builded. Outside
                         this area, the mask is set to 0.
        [-zoomij iimin iimax ijmin ijmax] : model grid windows used to
                         limit the area where the mask is builded. Outside
                         this area, the mask is set to 0.
        [-zoombat bathymin bathymax] : depth windows used to
                         limit the area where the mask is builded. Outside
                         this area, the mask is set to 0.
                         Need mesh_zgr.nc
        [-zoomvar varname varmin varmax] : range of varname variable used to
                         limit the area where the mask is builded. Outside
                         this area, the mask is set to 0.
        [-fill iiseed jjseed] : mask everything except the cells into the
                         non mask area where the point (iiseed,jjseed) is.
        [-filllonlat lon lat] : mask everything except the cells into the
                         non mask area where the point (lon,lat) is.
        [-bfij BOUND_IJ-file.txt] : Specify a boundary file expressed in
                         model grid point. It is used in conjunction with
                         -fill series of options, to define an arbitrary
                         closed area where mask values will be 1.
                         See format of boundary files in dedicated paragraph.
        [-bflonlat BOUND_LONLAT-file.txt] :  Specify a boundary file expressed 
                         in geographical coordinates. It is used in conjunction
                         with the -fill series of options, to define an 
                         arbitrary closed area, where mask values will be 1.
                         See format of boundary files in dedicated paragraph.
        [-time ] : Build a mask corresponding to each time step of the 
                        input file
        [-o OUT-file ] : output file name to be used in place of standard
                         name [ mask_sal.nc ]
       
      FORMAT OF BOUNDARY FILES :
        Boundary files describes a set of model sections forming a closed line.
        Each section (segment of the boundary) is defined by the position 
        (either in model I-J, or in geographical LON-LAT) of the ending points
        of the respective section. Therefore, each section in the boundary file
        corresponds to 2 lines :
        NAME
        xmin xmax ymin ymax logical_flag
        NAME2 
        .......
        EOF
          where xmin xmax ymin ymax specify the ending points of the section,
          and   logical_flag (either T ior F), indicates if the corresponding
          section is to be taken into account (T). 
        Note that last line of the file should be just 'EOF' 
       
       
      REQUIRED FILES :
        If option -zoombat is used, file mesh_zgr.nc is required.
        If option T-file is -maskfile then mask.nc is required.
        If option T-file is -mbathy then bathylevel.nc and mesh_zgr.nc
         are required.
       
      OUTPUT : 
        netcdf file : mask_sal.nc or OUT-file.
          variables : tmask, umask, vmask, fmask
                 fmask can differ from standard fmask because it does not
                 reflect the slip/noslip lateral condition.

