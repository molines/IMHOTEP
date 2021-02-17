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

for typ in votemper vosaline ; do
   ncdiff eORCA025.L75_5564_WOA18_1y_${typ}.nc4 eORCA025.L75_CLIM_WOA18_1y_${typ}.nc4  eORCA025.L75_5564-CLIM_WOA18_1y_${typ}.nc4
done

for typ in votemper vosaline ; do
   ncdiff eORCA025.L75_5564_WOA18_${typ}.nc4 eORCA025.L75_CLIM_WOA18_${typ}.nc4  eORCA025.L75_5564-CLIM_WOA18_1m_${typ}.nc4
done




exit

-rw-r--r-- 1 rcli002 cli  156205782 Feb 16 14:31 eORCA025.L75_5564_WOA18_1y_vosaline.nc4
-rw-r--r-- 1 rcli002 cli  203295060 Feb 16 14:23 eORCA025.L75_5564_WOA18_1y_votemper.nc4
-rw-r--r-- 1 rcli002 cli 1213788683 Feb 16 14:08 eORCA025.L75_5564_WOA18_vosaline.nc4
-rw-r--r-- 1 rcli002 cli 1723292897 Feb 16 14:06 eORCA025.L75_5564_WOA18_votemper.nc4
-rw-r--r-- 1 rcli002 cli  155559157 Feb 16 14:29 eORCA025.L75_CLIM_WOA18_1y_vosaline.nc4
-rw-r--r-- 1 rcli002 cli  203124502 Feb 16 14:27 eORCA025.L75_CLIM_WOA18_1y_votemper.nc4
-rw-r--r-- 1 rcli002 cli 1186357653 Feb 16 14:09 eORCA025.L75_CLIM_WOA18_vosaline.nc4
-rw-r--r-- 1 rcli002 cli 1700614683 Feb 16 14:11 eORCA025.L75_CLIM_WOA18_votemper.nc4

