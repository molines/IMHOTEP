# CONCATENATION of output files
## Context:
The quota for inode on the STORE system of Jean-Zay is limited.  We already have more than 880 000 allowed files which is more than 8 times the standard
quota.  IDRIS staff deny the requested increase, arguing that we should reduce the number of inodes for the STORE system.

## Reducing inodes:
  * For old archived runs, we decided to make big tar files ( some hundred of Gb) gathering either montly fields, or yearly fields depending on the
resolution of the configuration.
  * For active runs, we prefered to use netcdf concatenation, in order to have the files ready to use, even on the STORE system (btw, IDRIS stongly
encourages users NOT to work on STORE from compute node). This was done for 1d output files (concatenated in monthly files) and 5d output files 
(concatenated in yearly files). 

## Jobs :
  * concat1d.sh
  * concat5d.sh
  * wpi.sh2 : ( To be finished)
