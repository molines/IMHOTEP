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

ulimit -s unlimited

cd /gpfswork/rech/cli/rcli002/eORCA025.L75/eORCA025.L75-I
for f in eORCA025.L75_5564_WOA18_votemper.nc eORCA025.L75_5564_WOA18_vosaline.nc eORCA025.L75_CLIM_WOA18_vosaline.nc eORCA025.L75_CLIM_WOA18_votemper.nc ; do

ncks -O -3 $f tmp.nc
ncrename -d z,deptht tmp.nc
ncks -4 -L 1 --cnk_dmn deptht,1 tmp.nc ${f}4
echo done for $f
done

