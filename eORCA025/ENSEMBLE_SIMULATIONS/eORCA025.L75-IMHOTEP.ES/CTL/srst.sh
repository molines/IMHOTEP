#!/bin/bash
#SBATCH -J zsrst
#SBATCH --nodes=1
#SBATCH --ntasks=10
#SBATCH --ntasks-per-node=40
#SBATCH --time=0:20:00
#SBATCH -e zsrst.e%j
#SBATCH -o zsrst.o%j
#SBATCH -A cli@cpu
#SBATCH --exclusive
 set -x
 cd /gpfsscratch/rech/cli/rcli002/TMPDIR_eORCA025.L75-IMHOTEP.ES
 . ./includefile.sh
 . /gpfswork/rech/cli/rcli002/DEVGIT/DCM_4.0.6ens/RUNTOOLS/lib/function_4_all.sh
 . /gpfswork/rech/cli/rcli002/DEVGIT/DCM_4.0.6ens/RUNTOOLS/lib/function_4.sh
 srcbash # just in case
 # set local variables as in nemo4.sh (calling script)
if [ $# = 0 ] ; then 
   echo ' : sbatch srst.sh <extension>  '
   exit
fi
exit
 ext=$1
 AGRIF=0
 XIOS=1
 RST_DIRS=1
 nst_lst=" "

 mpmd_arg=""
 for member in $(seq 1 10) ; do
   mmm=$(getmember_extension $member)
   nnn=$(getmember_extension $member nodot )
   zrstdir='./'
   if [ $RST_DIRS = 1 ] ; then zrstdir=/gpfsscratch/rech/cli/rcli002/eORCA025.L75-IMHOTEP.ES-RST.$ext/$nnn ; fi
   # create secondary scripts to be submitted in // trhough the members
   # $ to be maintained in the final script are replaces by @, then automatic edition
   # replace the @ by $ [ this is necessary because we are at the second level of script
   # creation !
   cat << eof1 > ztmprst
#!/bin/bash
   set -x
   . ./includefile.sh
   . /gpfswork/rech/cli/rcli002/DEVGIT/DCM_4.0.6ens/RUNTOOLS/lib/function_4_all.sh
   . /gpfswork/rech/cli/rcli002/DEVGIT/DCM_4.0.6ens/RUNTOOLS/lib/function_4.sh

   # set local variables as in nemo3.4 (calling script)
   ext=18
   AGRIF=0
   XIOS=1
   nst_lst=" "
   mmm=$mmm
   zrstdir=$zrstdir
   cd /gpfsscratch/rech/cli/rcli002
   tar cf /gpfsstore/rech/cli/rcli002/eORCA025.L75/eORCA025.L75-IMHOTEP.ES-R/eORCA025.L75-IMHOTEP.ES${mmm}-RST.${ext}.tar eORCA025.L75-IMHOTEP.ES-RST.${ext}/$nnn
eof1
   cat ztmprst | sed -e 's/@/$/g' > ./zsrst.${ext}.sh${mmm}.sh    # change @ into $ and create script for current member
   chmod 755 ./zsrst.${ext}.sh${mmm}.sh                         # made it executable

   mpmd_arg="$mpmd_arg 1 ./zsrst.${ext}.sh${mmm}.sh"           # prepare the command line for runcode function
 done
  
  pwd
  if [ ! $mmm ] ; then
     ./zsrst.${ext}.sh${mmm}.sh                                 # not an ensemble run : serial process
  else
     runcode_mpmdOLD  $mpmd_arg                        # launch the scripts in parallele (mpmd mode)
  fi
