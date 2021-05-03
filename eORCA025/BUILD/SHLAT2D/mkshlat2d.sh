#!/bin/bash

CONFIG=eORCA025.L75


ln -sf ../${CONFIG}_mesh_mask.nc mask.nc
ln -sf ../${CONFIG}_mesh_mask.nc mesh_hgr.nc
ln -sf ../${CONFIG}_mesh_mask.nc mesh_zgr.nc

# use cdfmkresto build the 2D map of shlat2s
cat << eof > drakkar_shlat2d.txt
# Construction of shlat2d 
# Berring strait
# type lon1      lon2   lat1   lat2   rim   tau  z1  z2
   R    -172.3  -166.0  64.7   66.5   0      1   0   0
# Med Sea
# type lon1  lon2    lat1   lat2   rim   tau  z1  z2
   R   -5.7  42.7    29.5   42.9    0    1    0    0
   R    0.0  42.7    41.8   48.2    0    1    0    0
# West Greenland coast
# type lon   lat radius   rim   tau  z1  z2
   D   -48.9 61.5   100   30    1     0  0
   D   -49.8 62.1   100   30    1     0  0
   D   -50.3 62.7   100   30    1     0  0
   D   -51.2 63.4   100   30    1     0  0
   D   -51.8 64.0   100   30    1     0  0
# IJ patch formely defined in usrdef_fmask. Use exact IJ on F grid
# Gibraltar : 3
# type  imin   imax  jmin  jmax  val z1  z2
   I    1125   1127   838   840    3  0   0
# East Ombai Strait : 2 
# type  imin   imax  jmin  jmax  val z1  z2
   I     212    212   650   651    2  0   0
# West Ombai Strait : 2 
# type  imin   imax  jmin  jmax  val z1  z2
   I     210    211   652   652    2  0   0
# Exit of Ombai Strait : 2
# type  imin   imax  jmin  jmax  val z1  z2
   I     210    210   650   651    2  0   0
# Lombok Strait : 2
# type  imin   imax  jmin  jmax  val z1  z2
   I 	 172    175   649   649    2  0   0
# Torres Strait : 4
# type  imin   imax  jmin  jmax  val z1  z2
   I     278    279   642   647    4  0   0
eof

cdfmkresto -c mesh_hgr.nc -p F -i drakkar_shlat2d.txt -o  ${CONFIG}_shlat2d.nc -ov shlat2d -2d  -val 2 -nc4 

# not really interesting as points on F mask are masked ..
 cdfmltmask  -f ${CONFIG}_shlat2d.nc  -m mask.nc -v shlat2d -p F -noup


exit
  usage :  cdfmkresto -c COORD-file -i CFG-file [-d DEP-file] [-o DMP-file]...
                      ...[-ov VAR-out] [-2d] [-prev RESTO-file RESTO-var ] ...
                      ...[-p C-TYPE] [-val VALUE] [-nc4] [-h]
       
      PURPOSE :
        Create a file with a 3D damping coefficient suitable for the NEMO
        TS restoring performed by tradmp. Units for the damping_coefficient 
        are s^-1.
       
        The restoring zone is defined by a series of patches defined in the
        configuration file, and that can have either a rectangular or a circular
        shape. Overlapping patches are not added.
        
        This tool has been improved with new options (-val, -2d) so that it can
        be used for building 'bfr2d_coef' as well as 'shlat2d'.
       
      ARGUMENTS :
        -c COORD-file : pass the name of the file with horizontal coordinates.
        -i CFG-file : pass the name of an ascii configuration file used to 
                to define the restoring properties. (use cdfmkresto -h for 
                a detailed description of this CFG-file, with some examples.
       
      OPTIONS :
        [-h ]: print a detailed description of the configuration file.
        [-d DEP-file]: name on an ASCII file with gdept_1d, if mesh_zgr.nc
                 is not available. This file is just a list of the deptht, in
                 one column.
        [-prev RESTO-file RESTO-var] : Use RESTO-file and RESTO-var for the
                 initialization of the restoring coefficient. Units MUST be
                 s^-1 !!! 
        [-p C-type] : indicate on which grid point (T or F -so far-) the 
                variable is computed in the output file.
        [-val VALUE ] : with this option, the 'restoring' coefficient is
                   set to VALUE , instead of a time scale.
        [-2d ] : Create a 2D file instead of a default 3D file.
        [-o DMP-file]: name of the output file instead of damping_coef.nc.
        [-ov VAR-out]: name of the output variable instead of resto.
        [-nc4]  : Use netcdf4 output with chunking and deflation level 1
             This option is effective only if cdftools are compiled with
             a netcdf library supporting chunking and deflation.
       
      REQUIRED FILES :
          mesh_zgr.nc. If not available, use the -d option.
       
      OUTPUT : 
        netcdf file : damping_coef.nc unless -o option is used.
          variables : resto (s^-1 ) (unless -ov option used)
       
      SEE ALSO :
          cdfmkdmp

