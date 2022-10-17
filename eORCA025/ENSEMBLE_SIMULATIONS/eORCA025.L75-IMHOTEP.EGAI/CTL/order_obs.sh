#!/bin/bash


for n in {79..97} ; do

  echo process $n 
  cd $DDIR/eORCA025.L75-IMHOTEP.EGAI-DIAOBS.$n
     for mbr in {001..010} ; do
       mkdir -p $mbr
       mv profb.${mbr}_fdbk_????.nc ${mbr}/
     done
  cd ../
  echo $n ordered 
done
exit

profb.002_fdbk_0097.nc  profb.003_fdbk_0219.nc  profb.004_fdbk_0341.nc  profb.005_fdbk_0463.nc  profb.007_fdbk_0105.nc  profb.008_fdbk_0227.nc  profb.009_fdbk_0349.nc  profb.010_fdbk_0471.nc
profb.002_fdbk_0098.nc  profb.003_fdbk_0220.nc  profb.004_fdbk_0342.nc  profb.005_fdbk_0464.nc  profb.007_fdbk_0106.nc  profb.008_fdbk_0228.nc  profb.009_fdbk_0350.nc  profb.010_fdbk_0472.nc
profb.002_fdbk_0099.nc  profb.003_fdbk_0221.nc  profb.004_fdbk_0343.nc  profb.005_fdbk_0465.nc  profb.007_fdbk_0107.nc  profb.008_fdbk_0229.nc  profb.009_fdbk_0351.nc  profb.010_fdbk_0473.nc
profb.002_fdbk_0100.nc  profb.003_fdbk_0222.nc  profb.004_fdbk_0344.nc  profb.005_fdbk_0466.nc  profb.007_fdbk_0108.nc  profb.008_fdbk_0230.nc  profb.009_fdbk_0352.nc  profb.010_fdbk_0474.nc
profb.002_fdbk_0101.nc  profb.003_fdbk_0223.nc  profb.004_fdbk_0345.nc  profb.005_fdbk_0467.nc  profb.007_fdbk_0109.nc  profb.008_fdbk_0231.nc  profb.009_fdbk_0353.nc  profb.010_fdbk_0475.nc
profb.002_fdbk_0102.nc  profb.003_fdbk_0224.nc  profb.004_fdbk_0346.nc  profb.005_fdbk_0468.nc  profb.007_fdbk_0110.nc  profb.008_fdbk_0232.nc  profb.009_fdbk_0354.nc  profb.010_fdbk_0476.nc
profb.002_fdbk_0103.nc  profb.003_fdbk_0225.nc  profb.004_fdbk_0347.nc  profb.005_fdbk_0469.nc  profb.007_fdbk_0111.nc  profb.008_fdbk_0233.nc  profb.009_fdbk_0355.nc  profb.010_fdbk_0477.nc
profb.002_fdbk_0104.nc  profb.003_fdbk_0226.nc  profb.004_fdbk_0348.nc  profb.005_fdbk_0470.nc  profb.007_fdbk_0112.nc  profb.008_fdbk_0234.nc  profb.009_fdbk_0356.nc  profb.010_fdbk_0478.nc
profb.002_fdbk_0105.nc  profb.003_fdbk_0227.nc  profb.004_fdbk_0349.nc  profb.005_fdbk_0471.nc  profb.007_fdbk_0113.nc  profb.008_fdbk_0235.nc  profb.009_fdbk_0357.nc  profb.010_fdbk_0479.nc
profb.002_fdbk_0106.nc  profb.003_fdbk_0228.nc  profb.004_fdbk_0350.nc  profb.005_fdbk_0472.nc  profb.007_fdbk_0114.nc  profb.008_fdbk_0236.nc  profb.009_fdbk_0358.nc
profb.002_fdbk_0107.nc  profb.003_fdbk_0229.nc  profb.004_fdbk_0351.nc  profb.005_fdbk_0473.nc  profb.007_fdbk_0115.nc  profb.008_fdbk_0237.nc  profb.009_fdbk_0359.nc
profb.002_fdbk_0108.nc  profb.003_fdbk_0230.nc  profb.004_fdbk_0352.nc  profb.005_fdbk_0474.nc  profb.007_fdbk_0116.nc  profb.008_fdbk_0238.nc  profb.009_fdbk_0360.nc
profb.002_fdbk_0109.nc  profb.003_fdbk_0231.nc  profb.004_fdbk_0353.nc  profb.005_fdbk_0475.nc  profb.007_fdbk_0117.nc  profb.008_fdbk_0239.nc  profb.009_fdbk_0361.nc
profb.002_fdbk_0110.nc  profb.003_fdbk_0232.nc  profb.004_fdbk_0354.nc  profb.005_fdbk_0476.nc  profb.007_fdbk_0118.nc  profb.008_fdbk_0240.nc  profb.009_fdbk_0362.nc
profb.002_fdbk_0111.nc  profb.003_fdbk_0233.nc  profb.004_fdbk_0355.nc  profb.005_fdbk_0477.nc  profb.007_fdbk_0119.nc  profb.008_fdbk_0241.nc  profb.009_fdbk_0363.nc
(base) cli [rcli002@jean-zay3: eORCA025.L75-IMHOTEP.EGAI-DIAOBS.78]$ cd ..
(base) cli [rcli002@jean-zay3: rcli002]$ ls
5D_AVERAGE                    NNATL12                              eORCA025.L75-IMHOTEP.EGAI-DIAOBS.79  eORCA025.L75-IMHOTEP.EGAI-DIAOBS.98  eORCA025.L75-IMHOTEP.EGAI-ICB.96   eORCA025.L75-IMHOTEP.ES-RST.101
ANA                           ORCA2                                eORCA025.L75-IMHOTEP.EGAI-DIAOBS.80  eORCA025.L75-IMHOTEP.EGAI-ICB.78     eORCA025.L75-IMHOTEP.EGAI-ICB.97   eORCA025.L75-IMHOTEP.ES-RST.102
ANA2                          PLOT_GREENLAND                       eORCA025.L75-IMHOTEP.EGAI-DIAOBS.81  eORCA025.L75-IMHOTEP.EGAI-ICB.79     eORCA025.L75-IMHOTEP.EGAI-ICB.98   eORCA025.L75-IMHOTEP.ES-RST.103
Arctic                        PSI                                  eORCA025.L75-IMHOTEP.EGAI-DIAOBS.82  eORCA025.L75-IMHOTEP.EGAI-ICB.80     eORCA025.L75-IMHOTEP.EGAI-RST.92   eORCA025.L75-IMHOTEP.ES-RST.104
DOMAIN_cfg                    STERIC                               eORCA025.L75-IMHOTEP.EGAI-DIAOBS.83  eORCA025.L75-IMHOTEP.EGAI-ICB.81     eORCA025.L75-IMHOTEP.EGAI-RST.93   eORCA025.L75-IMHOTEP.ES-RST.77
FORCING_TEST_IMHOTEP.ES       TMPDIR_MED025.L75-GJMens             eORCA025.L75-IMHOTEP.EGAI-DIAOBS.84  eORCA025.L75-IMHOTEP.EGAI-ICB.82     eORCA025.L75-IMHOTEP.EGAI-RST.94   eORCA025.L75-IMHOTEP02-RST.17
FORCING_eNATL36X-WRF          TMPDIR_eORCA025.L75-IMHOTEP.EGAI     eORCA025.L75-IMHOTEP.EGAI-DIAOBS.85  eORCA025.L75-IMHOTEP.EGAI-ICB.83     eORCA025.L75-IMHOTEP.EGAI-RST.95   eORCA05.L121
GSW-test                      TMPDIR_eORCA025.L75-IMHOTEP.ES       eORCA025.L75-IMHOTEP.EGAI-DIAOBS.86  eORCA025.L75-IMHOTEP.EGAI-ICB.84     eORCA025.L75-IMHOTEP.EGAI-RST.96   eORCA12.L75
HiRes                         TMPDIR_eORCA025.L75-IMHOTEP.S        eORCA025.L75-IMHOTEP.EGAI-DIAOBS.87  eORCA025.L75-IMHOTEP.EGAI-ICB.85     eORCA025.L75-IMHOTEP.EGAI-RST.97   toto
ISF                           TMPDIR_eORCA025.L75-IMHOTEP.SC       eORCA025.L75-IMHOTEP.EGAI-DIAOBS.88  eORCA025.L75-IMHOTEP.EGAI-ICB.86     eORCA025.L75-IMHOTEP.EGAI-RST.98   toto.f90
JMB                           TMPDIR_eORCA025.L75-IMHOTEP01        eORCA025.L75-IMHOTEP.EGAI-DIAOBS.89  eORCA025.L75-IMHOTEP.EGAI-ICB.87     eORCA025.L75-IMHOTEP.EGAI-XIOS.90  toto~
JRA55_1d_uva                  TSTJM                                eORCA025.L75-IMHOTEP.EGAI-DIAOBS.90  eORCA025.L75-IMHOTEP.EGAI-ICB.88     eORCA025.L75-IMHOTEP.EGAI-XIOS.91  tstalpha
MED025.L75                    WK-EOS                               eORCA025.L75-IMHOTEP.EGAI-DIAOBS.91  eORCA025.L75-IMHOTEP.EGAI-ICB.89     eORCA025.L75-IMHOTEP.EGAI-XIOS.92  xios-2.5
MED025.L75-GJM407e-DIAOBS.10  WOA2018                              eORCA025.L75-IMHOTEP.EGAI-DIAOBS.92  eORCA025.L75-IMHOTEP.EGAI-ICB.90     eORCA025.L75-IMHOTEP.EGAI-XIOS.93  xios-2.5@1869
MED025.L75-GJMens-RST.18      WRFATL12-now2                        eORCA025.L75-IMHOTEP.EGAI-DIAOBS.93  eORCA025.L75-IMHOTEP.EGAI-ICB.91     eORCA025.L75-IMHOTEP.EGAI-XIOS.94
MONITOR_eORCA12.L75-GJM2020   dcmtk_mvnc2s_mvfast                  eORCA025.L75-IMHOTEP.EGAI-DIAOBS.94  eORCA025.L75-IMHOTEP.EGAI-ICB.92     eORCA025.L75-IMHOTEP.EGAI-XIOS.95
MONITOR_eORCA12.L75-GJMFK21   eNATL36X                             eORCA025.L75-IMHOTEP.EGAI-DIAOBS.95  eORCA025.L75-IMHOTEP.EGAI-ICB.93     eORCA025.L75-IMHOTEP.EGAI-XIOS.96
MONTAGE                       eORCA025.L75                         eORCA025.L75-IMHOTEP.EGAI-DIAOBS.96  eORCA025.L75-IMHOTEP.EGAI-ICB.94     eORCA025.L75-IMHOTEP.EGAI-XIOS.97
NATLORCA12.L75                eORCA025.L75-IMHOTEP.EGAI-DIAOBS.78  eORCA025.L75-IMHOTEP.EGAI-DIAOBS.97  eORCA025.L75-IMHOTEP.EGAI-ICB.95     eORCA025.L75-IMHOTEP.EGAI-XIOS.98

