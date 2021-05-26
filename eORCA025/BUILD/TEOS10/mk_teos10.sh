#!/bin/bash
# convert potential temperature, practical salinity files to
#  conservative temperature, absolute salinity files
CONFIG=eORCA025.L75                  # Configuration name
IDIR=$WORK/${CONFIG}/${CONFIG}-I     # -I directory
build=$IDIR/TEOS-10_build            # working directory for TEOS10 conversion
EOS80DIR=$IDIR/POTENTIAL-PRACTICAL   # directory with potential temperature and practical salinity files
                                     # assuming names such as <CONFIG>_*_votemper.nc and <CONFIG>_*_vosaline.nc

mkdir -p $build
cd $build
ln -sf ${EOS80DIR}/*.nc ./
if [ ! -f $DEVGIT/JMMTOOLS/DATA_TOOLS/LEVITUS-WOA/gsw_data_v3_0.dat ] ; then
  echo "Cannot find gsw_data_v3_0.dat in $DEVGIT"
  echo " Check for JMMTOOLS/DATA_TOOLS/LEVITUS-WOA"
  exit
else
  ln -sf $DEVGIT/JMMTOOLS/DATA_TOOLS/LEVITUS-WOA/gsw_data_v3_0.dat ./
fi
if [ ! -f $DEVGIT/JMMTOOLS/DATA_TOOLS/LEVITUS-WOA/pt_sp_to_ct_sa ] ; then
  echo "Cannot find pt_sp_to_ct_sa  in $DEVGIT"
  echo " Check for JMMTOOLS/DATA_TOOLS/LEVITUS-WOA/"
  echo "  Compilation ? "
  exit
else
  ln -sf $DEVGIT/JMMTOOLS/DATA_TOOLS/LEVITUS-WOA/pt_sp_to_ct_sa ./
fi

for sp in ${CONFIG}_*_vosaline*.nc ; do
   pt=$( echo $sp | sed -e 's/vosaline/votemper/')
   ct=$( echo $sp | sed -e 's/vosaline/CT/')
   sa=$( echo $sp | sed -e 's/vosaline/SA/')

   ./pt_sp_to_ct_sa -pt $pt -vpt votemper -sp $sp -vsp vosaline \
     -ct $ct -vct CT -sa $sa -vsa SA -x x -y y -z deptht -t time_counter \
     -lon nav_lon -lat nav_lat  -dep deptht -tim time_counter -2D

done

exit
 USAGE :  pt_sp_to_ct_sa  -pt IN-pt -vpt VAR-pt -sp IN-sp -vsp VAR-sp 
          -ct OUT-CT  -vct VAR-CT  -sa  OUT-sa -vsa VAR-SA 
          [-x X-dim ] [-y Y-dim] [-z Z-dim] [-lon VAR-lon] [-lat VAR-lat] 
          [-dep VAR-dep ] [-tim VAR-time] [-2D]
  
  Purpose :
      Compute CT and SA (TEOS10) from potential temperature and practical 
      salinity
  
  Argument:
       -pt  IN-pt  : input potential temperature file
       -vpt VAR-pt : input potential temperature variable
       -sp  IN-sp  : input practical salinity file
       -vsp VAR-sp : input practical salinity  variable
       -ct  OUT-ct : output conservative temperature file
       -vct VAR-ct : output conservative temperature variable
       -sa  PUT-sa : output absolute salinity file
       -vsa VAR-sa : output absolute salinity variable
  
  Options:
       -x X-dim    : X-dimension name [lon]
       -y Y-dim    : Y-dimension name [lat]
       -z Z-dim    : Z-dimension name [depth]
       -t T-dim    : T-dimension name [time]
       -lon VAR-lon  : name of longitude variable [lon]
       -lat VAR-lat  : name of latitude variable [lat]
       -dep VAR-dep  : name of depth variable [depth]
       -tim VAR-time : name of time variable [time]
       -2D           :  use 2D lon lat variables [ F ]
  
  Requested files :
     gsw_data_v3_0.dat : tabulated values for GSW toolbox. 
        Can be found in JMM_TOOLS/DATA_TOOLS/LEVITUS-WOA
