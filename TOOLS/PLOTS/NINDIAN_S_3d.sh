#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH --hint=nomultithread
#SBATCH -A cli@cpu
#SBATCH -J JOB_
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=2:00:00
#SBATCH --exclusive

set -x
module load imagemagick/7.0.8-7

CONFIG=eORCA025.L75
CASE=IMHOTEP.EGAI.001

freq=1d
CONFCASE=${CONFIG}-${CASE}
DTADIR=/gpfsscratch/rech/cli/rcli002/eORCA025.L75/eORCA025.L75-IMHOTEP.EGAI-ENSCLIM-MEAN

RM=echo
vp="37 -16.705 136. 48.176"
#zoom="2190 2000 3900 3450"
zoom="2 2 390 294"
nmax=33  # max number of simultaneous task

# common to all plots
width=8   # Plot frame in inches
height=6
res=i     # resolution of the coast line c l i h f 
charpal=blu_red
proj=merc     # merc cyl 
bckgrd=shadedrelief   # none etopo shadedrelief bluemarble
depv="deptht"
xstep=10
ystep=10

y1=1982
y2=2018

for var in sss_mean  ; do
    case $var in
    (sosaline)
       title="BoB_SSS" ;;
    (sss_mean)
       title="Nindian_SSS_ens Anomaly" ;;
    (somxl010)
       title="ATL_MLDrho0.01" ;;
    (somxl020)
       title=".ATL_MLDrho0.03" ;;
    (somxlt02)
       title="ATL_MLDtem0.20" ;;
    esac

    figs=./fig_NINDIAN_egai_sss_anomaly_$var
    vmin=-3
    vmax=3
    tick=0.5
    klev=-1
    mkdir -p $figs

    n=0
    for y in $(seq $y1 $y2 ) ; do
    for m in {01..12}; do

#	for f in $DTADIR/${y}-concat/${CONFCASE}_y${y}m??_1d_gridTsurf.nc ; do
	for f in $DTADIR/NINDIAN025.L75-IMHOTEP.EGAI_y${y}m${m}.1m_sssanom.nc ; do
	   ff=$(basename $f )
	   g=${ff%.nc} 
	   if [ ! -f $figs/$g.png ] ; then
              ( ln -sf $f $ff
	        python_plot.py -i $g -v $var  -pc $charpal -proj $proj -xstep $xstep -ystep $ystep \
	       -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
               -figsz $width $height -res $res  -tick $tick
	       $RM ./$ff  ) &
	      
	       n=$(( n + 1 ))
	       if [ $n = $nmax ] ; then
		  wait
		  n=0
	       fi
	   else 
	       echo $g.png already done
	   fi
	done
    done
    done
    wait

    cd $figs
    for f in ${CONFCASE}*.png ; do
	convert $f ${f%.png}.gif
    done
    gifsicle -d0 -l0  ${CONFCASE}*.gif > ${CONFIG}_${title}_MONITOR-$CASE.gif
    cd ../
done # loop on var

