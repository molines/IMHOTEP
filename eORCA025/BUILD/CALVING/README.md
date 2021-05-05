# Building the Calving input file for NEMO
## 1. Context
We decided to use explicit representation of the freshwater fluxes coming from iceberg melting. This is available in NEMO 
using the ICB module (Marsh et al, 2015).  In this case the user needs to specify the calving rate at some  points along 
the coastline. Data are passed to NEMO through a file, giving this rate for each iceshelf (or marine glacier) taken into 
consideration in the configuration.

## 2. Data set
Preparation of this file is a long process: From one side we have published values (for instance Rignot et Al, 2016, 
for Antarctica, or Mouginot et al, personnal comm. for Greenland). And on the other side we must distribute this rate 
along the iceshelves. 

**For Greenland**, Jeremie Mouginot and Pierre Mathiot directly provided the points of the NEMO grid corresponding to 
calving sources (and corresponding calving rates).  Data from Jeremie are monthly data. (See comments in 
[this document](../RUNOFF_GREENLAND/README.md)).

**For Antarctica**, we took the file prepared by Pierre Mathiot, in which a clever algorithm was used to randomly distribute 
the  calving rate: for each iceshelf a single integral value is provided; at every point along the iceshelf break, a calving 
point is set with a calving rate being a random fraction of the global rate for this shelf. Of course, a scaling is applied 
in order to recover the global rate when summing up all individual points. Data consist in an annual climatology. A bash
[script](./get_calving_file_pm.sh), carry out the following task:
  * retrieve original file from MEOM-OPENDAP.
  * add nav_lon, nav_lat, time_counter variable in order to be CDFTOOL complient.
  * Set to 0 all data in the northern hemisphere.
  * Compress and rename file to its final name.

## 3. Dealing with data with different frequency.
We are faced to the problem of different frequencies for calving data in the southern and northern hemisphere. A straight forward workaround is to replicate
annual data on 12 month to form a pseudo monthly file.  A smarter way is to make NEMO able to read various data set (defined in the namelist), each with its own
frequency.  At the end, `fldread` does the job to provide a `now` field for each of the original data set,  which will be cumulated when used in NEMO.

This latter technique can also be used for runoff and iceshelf melting  data. (See [dedicated note](../../../Doc/Multiple_frequency_runoff.md).)
