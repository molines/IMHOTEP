#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH -A cli@cpu
#SBATCH --hint=nomultithread
#SBATCH -J JOB_
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=0:30:00
#SBATCH --exclusive

CONFIG=eORCA025.L75
ulimit -s unlimited

mkdir -p $WORK/${CONFIG}/${CONFIG}-I/GOURETSKI_2018
cd $WORK/${CONFIG}/${CONFIG}-I/GOURETSKI_2018

ln -sf ../${CONFIG}_mesh_mask.nc ./
ln -sf $WORK/DATA_SET/GOURETSKI_2018/WAGHC_S_BAR_1m_UHAM-ICDC_v1_0_1.nc ./
ln -sf $WORK/DATA_SET/GOURETSKI_2018/WAGHC_THETA0_BAR_1m_UHAM-ICDC_v1_0_1.nc ./

ln -sf $DEVGIT/sosie-3.0-base/bin/*.x ./

# sosie for theta0
./sosie3.x -f  namelist_theta_gouretski  &
# sosie for salinity
./sosie3.x -f  namelist_salinity_gouretski &

wait


# rename file to their final name 
for var in vosaline votemper; do
  for f in ${var}_GOURETSKI_18-${CONFIG}_climatology.nc ; do
      mv $f ${CONFIG}_GOU18_1m_${var}.nc
  done
done

for var in vosaline votemper ; do
    ( f=${CONFIG}_GOU18_1m_${var}.nc 
    ncks -O -3 $f tmp.nc
    ncrename -v glamt,nav_lon -v gphit,nav_lat -d z,deptht tmp.nc
    ncks -O -4 -L 1 --cnk_dmn deptht,1 tmp.nc ${f}4
    rm tmp.nc
    echo done for $f ) &
done

wait
