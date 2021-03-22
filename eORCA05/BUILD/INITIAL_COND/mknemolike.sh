#!/bin/bash
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

CONFIG=eORCA05.L75
ulimit -s unlimited
PERIOD=81B0

cd /gpfswork/rech/cli/rcli002/${CONFIG}/${CONFIG}-I/INITIAL_COND_build

# rename file to their final name 
for var in vosaline votemper t_an; do
   for f in ${var}_WOA18_1deg-${CONFIG}_climatology.nc ; do
       mv $f ${CONFIG}_${PERIOD}_WOA18_1m_${var}.nc
   done
done

for var in vosaline votemper t_an; do
   for f in  ${CONFIG}_${PERIOD}_WOA18_1m_${var}.nc ; do
    ncks -O -3 $f tmp.nc
    ncrename -v glamt,nav_lon -v gphit,nav_lat -d z,deptht tmp.nc
    ncks -O -4 -L 1 --cnk_dmn deptht,1 tmp.nc ${f}4
    rm tmp.nc
    echo done for $f
   done
done
