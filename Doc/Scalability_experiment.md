# How to carry out a scalability experiment
## 1. Context
When using a massively parallel (MPP) machine, it is of primary interest to evaluate the scalability of the numerical
code. In MPP computing, when using domain decomposition techniques, a computing core is associated to a physical
subdomain. Subdomains (hence core) exchange information with the neighbours, in a halo with overlapping points.
The more subdomains, the less grid points to compute per domain (domains are smaller) but the more inter core
communications. Therefore there is a trade-off between computing burden, and communication. Scalability experiments
aim at measuring the performance of a code when increasing the number of cores dedicated to the computation. The
actual performances are compared with a theoretical pragmatic estimates, corresponding to "perfect" scalability
(linear increase of performances with respect to the number of cores).
## 2. Implementation with DCM. 
### 2.1 Measuring the performances
Measuring the actual performances of the code is the key point for scalability experiment. We implement in NEMO (DCM)
a very easy way to perform this measure:  a time-tag is printed in the job output file, at every begining of a time step
in NEMO. Then with post-processing tools, we are able to do a lot of things with this information, and in particular
we can evaluate the performance of the code, measured in step/min. Although not sophisticated at all, this technique
is really efficient for the scalability experiments purposes.
### 2.2 Workflow of the experiment
The idea is to repeat a short experiment (here we took 3-days experiment), changing only the number of cores used for NEMO.
With NEMO4/DCM, the total number of core used for NEMO is passed to the machine as an argument of the `srun` (SLURM) or 
equivalent command. Using XIOS server, in fact there are 2 executable launched by the command, `nemo4.exe` and `xios_server.exe`.
For the scalability experiment we fix the number of xios_server to be 4.   
A particularity of `jean-zay@IDRIS` (target HPC),
is that xios_server cannot be on a same computing node than NEMO (reason is still unclear, but this is a fact).  Computing
nodes have 40 cores available. Therefore, the scalability experiment will explore domain decomposition being multiple of 40.  


With NEMO/DCM the way of launching a standard segment of run is quite simple: each configuration is associated to a control directory (CTL)
in which template namelists (ocean and ice), template xml files (for XIOS), path definition file (include_file.sh), and the 
script file to be launched (eORCA025.L75-IMHOTHEP00_jean-zay.sh).  An extra file (eORCA025.L75-IMHOTHEP00.db) is used to manage the 
time steps to perform in a segment.  Once all these files are set up, launching a segment is done  with `run_nemo.sh`. 

For scalability experiment, the procedure is very similar, but there are more templates files in order to let the number of cores for NEMO
being a variable. In order to ease the post-processing, and also in  order to be able to run different cases (number of cores) simultaneously,
all input files and scripts have a unique ID indicating the corresponding case.   A bash script ([run_nemo.scal.sh](....)) is used as  a
wrapper to launch each case:

```
USAGE: run_nemo.scal.sh [-h] [-c CORES-per-node] jpni jpnj jpnij nxios
  
  PURPOSE:
    Launch scalability experiment corresponding to domain decomposition given
    in the arguments.
  
  ARGUMENTS:
    jpni : number of subdomains in the I-direction
    jpnj : number of subdomains in the J-direction
    jpnij : Total number of ocean only subdomains.
    nxios : Number of xios_server.exe to be launched.
  
  OPTIONS:
    -h : print this usage message.
    -c CORES-per-node : set number of cores per computing node. Default : 40
```

On jean-zay, and NEMO4, nxios is fixed to 4 and the only relevant argument is jpnij. A meta script is even written to pack all the run_nemo.scal.sh commands
([runscal.sh](...)). It looks like :

```
#!/bin/bash
./run_nemo.scal.sh -c 40 36 9 240 4
./run_nemo.scal.sh -c 40 19 20 280 4
./run_nemo.scal.sh -c 40 23 22 360 4
./run_nemo.scal.sh -c 40 43 13 400 4
./run_nemo.scal.sh -c 40 37 17 440 4
./run_nemo.scal.sh -c 40 23 30 480 4
./run_nemo.scal.sh -c 40 39 19 520 4
./run_nemo.scal.sh -c 40 15 53 560 4
./run_nemo.scal.sh -c 40 36 26 640 4
./run_nemo.scal.sh -c 40 50 20 680 4
...
./run_nemo.scal.sh -c 40 46 76 2200 4
./run_nemo.scal.sh -c 40 44 81 2240 4
./run_nemo.scal.sh -c 40 61 60 2280 4
./run_nemo.scal.sh -c 40 103 36 2320 4
./run_nemo.scal.sh -c 40 60 63 2360 4
./run_nemo.scal.sh -c 40 82 47 2400 4
```

## 2.3 Post-processing of the output-file.
Each individual submitted case, ends up with a job output file (for example: nemo_jean-zay_50_20_680.o1295227). Within
this job output file the time tags corresponding to each NEMO step are written :

```
(4) Run the code
----------------
Thu May  6 11:09:23 CEST 2021
       1 2021   5   6 120  11   9  35 856
       2 2021   5   6 120  11   9  55 180
       3 2021   5   6 120  11   9  55 638
       4 2021   5   6 120  11   9  56  99
       5 2021   5   6 120  11  10   3 697
       6 2021   5   6 120  11  10   4 163
       7 2021   5   6 120  11  11   3 358
       8 2021   5   6 120  11  11   3 822
       9 2021   5   6 120  11  11   4 281
      10 2021   5   6 120  11  11   4 750
      11 2021   5   6 120  11  11   5 227
      12 2021   5   6 120  11  11   5 691
      13 2021   5   6 120  11  11   6 155
....
     205 2021   5   6 120  11  12  50 825
     206 2021   5   6 120  11  12  51 300
     207 2021   5   6 120  11  12  51 772
     208 2021   5   6 120  11  12  52 251
     209 2021   5   6 120  11  12  52 734
     210 2021   5   6 120  11  12  53 209
     211 2021   5   6 120  11  12  53 686
     212 2021   5   6 120  11  12  54 161
     213 2021   5   6 120  11  12  54 639
     214 2021   5   6 120  11  12  55 163
     215 2021   5   6 120  11  12  55 654
Thu May  6 11:13:58 CEST 2021
```

In DCM toolkit, script are developped to screen this time information (as usual, usage message is issued using the tool with -h option).
  * **dcmtk_timeevol** : display a graph showing  the number of steps performed as a function of elapsed time.  
Example: `dcmtk_timeevol nemo_jean-zay_50_20_680.o1295227 5 250` gives the following plot:  

   <img src=./Figures/nemo_jean-zay_50_20_680_scal.gif />   
   
On this plot, corresponding to the case 680 cores, there are 216 time steps, corresponding to 3x72 steps, 
says 3 days. During the first day, initialization of NEMO and XIOS clearly dramatically pull down the performances. 
Then around step 72, the impact of the fisrt XIOS write on file is visible. It is much smoother at the end of the 
second day (step 144). Performance of the experiment will be evaluated as the slope of the curve for the last 72 steps (last day).
  * **dcmtk_rate** is the script that analyse the above curve and gives evaluation on the performance. Note that this script skip the    
first and last 10 points in a segment, in order to avoid side effects of initialisation and completion of a segment.  
Example: `dcmtk_rate -s 72 -b 3 -f nemo_jean-zay_50_20_680.o1295227` gives :

  ```
   $ dcmtk_rate -s 72 -b 3 -f nemo_jean-zay_50_20_680.o1295227 
   1 RATE :   123.40476562687948      step/min
   2 RATE :   122.92700781854866      step/min
   3 RATE :   122.47403083569577      step/min
  ```

  * **dcmtk_chkrate_scal** is a wrapper for the dcmtk_rate script that scans all the nemo-jean-zay_xxx.oxxx file in the current directory and output
a table with the 4 columns : jpni jpnj jpnij peformance (step/min). This table is the result of the scalability experiment.  
Example: `dcmtk_chkrate_scal  go`

  ```
  103 36 2320   370.77832798846680
  15 53 560   94.629064202140952
  19 20 280   49.479795668176301
  23 22 360   63.163505453658708
  23 30 480   81.233461054198159
  23 49 760   137.04560287889612
  29 59 1120   184.40073663990236
  ....
  ```  
Results can be pasted into a spread-sheet in order to follow sorting results,  with graphs and extra performance information such as efficiency, etc ...  
Python geeks may probably be tempted to build a program that does all at once... However, the present system had the advantage of using only very portable scripts, running
on any UNIX machine.




