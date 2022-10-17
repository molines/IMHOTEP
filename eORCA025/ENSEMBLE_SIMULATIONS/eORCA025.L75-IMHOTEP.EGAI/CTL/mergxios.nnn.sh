#!/bin/bash
#SBATCH -J zmergxios
#SBATCH --nodes=5
#SBATCH --ntasks=60
#SBATCH --ntasks-per-node=40
#SBATCH --time=2:00:00
#SBATCH -e zmergxios.e%j
#SBATCH -o zmergxios.o%j
#SBATCH -A cli@cpu
#SBATCH --exclusive

        if [ $# = 0 ] ; then
            echo "  USAGE : sbatchd mergexios_nnn.sh  SEGMENT"
            exit 0
        fi
        n=$1


        set -x 
        ulimit -s unlimited
      . /gpfswork/rech/cli/rcli002/DEVGIT/DCM_4.0.6ens/RUNTOOLS/lib/function_4.sh
      . /gpfswork/rech/cli/rcli002/DEVGIT/DCM_4.0.6ens/RUNTOOLS/lib/function_4_all.sh
         DDIR=/gpfsscratch/rech/cli/rcli002
         zXIOS=/gpfsscratch/rech/cli/rcli002/eORCA025.L75-IMHOTEP.EGAI-XIOS.${n}
         mergeprog=mergefile_mpp4.exe
         cd $zXIOS
         ln_ensemble=0
         mbrlist="./"
         if [ -d 001 ] ; then
            ln_ensemble=1
            mbrlist=$( ls -d ??? ) 
         fi
      for mbr in $mbrlist ; do
         cd $zXIOS/$mbr
         if [ $mbr != "./" ] ; then
            ln -sf ../eORCA025.L75_domain_cfg_closed_seas_greenland.nc ./
         fi
     
         # deal with scalar files
         ls *scalar*0000.nc > /dev/null  2>&1
         if [ $? = 0 ] ; then
            mkdir -p SCALAR
            mv *scalar*.nc SCALAR
            cd SCALAR
            set +x
              for f in *scalar*_0000.nc ; do
                 CONFCASE=$( echo $f | awk -F_ '{print $1}' )
                 freq=$( echo $f | awk -F_ '{print $2}' )
                 freq_unit=${freq:1}
                 tag=$( echo $f | awk -F_ '{print $5}' | awk -F- '{print $1}' )

                 case  $freq_unit  in
                 ('m') date=y${tag:0:4}m${tag:4:2} ;;
                 ('y') date=y${tag:0:4} ;;
                 ( * ) date=y${tag:0:4}m${tag:4:2}d${tag:6:2} ;;
                 esac

                 typ=$(echo $f | awk -F_ '{ print $3}')
                 case $typ in
                 ( 'SBC' ) filext='icescalar' ;;
                 ( 'FWB' ) filext='fwbscalar' ;;
                 ( *     ) filext='unkscalar' ;;
                 esac

                 g=${CONFCASE}_${date}.${freq}_${filext}.nc
                 OUTDIR=../${freq}_OUTPUT
                 mkdir -p $OUTDIR
                 cp $f $OUTDIR/$g

              done
         # end scalar file
            set -x
            cd  $zXIOS/$mbr

         fi
         ln -sf /gpfswork/rech/cli/rcli002/bin/mergefile_mpp4.exe ./
             runcode 60 ./$mergeprog -F -c eORCA025.L75_domain_cfg_closed_seas_greenland.nc -r
      done
