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
#SBATCH --time=1:00:00
#SBATCH --exclusive

ulimit -s unlimited
cd /gpfswork/rech/cli/rcli002/eORCA025.L75/eORCA025.L75-I/WOA2018_Assessment

PERIOD=(5564 81B0 CLIM )
echo ${PERIOD[@]}
np=${#PERIOD[@]}
imax=$(( np - 1 ))

for i in $( seq 0 $imax ) ; do
    ideb=$(( i + 1 ))
   for j in  $(seq $ideb $imax ) ; do
     echo ${PERIOD[$i]} ${PERIOD[$j]}
     P1=${PERIOD[$i]}
     P2=${PERIOD[$j]}
     
     if [ $P1 = '5564' -a $P2 = 'CLIM' ] ; then
       echo skip
     else

     for typ in votemper vosaline ; do
        ncdiff eORCA025.L75_${P1}_WOA18_1y_${typ}.nc eORCA025.L75_${P2}_WOA18_1y_${typ}.nc  eORCA025.L75_${P1}-${P2}_WOA18_1y_${typ}.nc
        ncks -A -v nav_lon,nav_lat eORCA025.L75_${P1}_WOA18_1y_${typ}.nc eORCA025.L75_${P1}-${P2}_WOA18_1y_${typ}.nc
     done

     for typ in votemper vosaline ; do
        ncdiff eORCA025.L75_${P1}_WOA18_1m_${typ}.nc eORCA025.L75_${P2}_WOA18_1m_${typ}.nc  eORCA025.L75_${P1}-${P2}_WOA18_1m_${typ}.nc
        ncks -A -v nav_lon,nav_lat eORCA025.L75_${P1}_WOA18_1m_${typ}.nc eORCA025.L75_${P1}-${P2}_WOA18_1m_${typ}.nc
     done
     fi


   done
done

