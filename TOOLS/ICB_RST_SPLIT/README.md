# ICB_RST_SPLIT tool
## Context:
  When changing the number of core used in the computation, iceberg restart files must be changed in order to reflect the new domain decomposition.
Fields in Iceberg restart files are not gridded fields, hence traditional tools for recombining domain splitted files are not adecuate.

A specific tool has been developped for this purpose: `icbrstsplit.exe`

## Method
  * The used method  assumes that the new domain decomposition is already known and a layout.dat file is therefore available.
  * The bunch of existing file is read and used to fill global arrays.
  * Reading `layout.dat` file, new iceberg restart files are created (one for each subdomain) and a screening on iceberg position is performed in
order to fill every subdomain file with the *ad hoc* information.

## Compilation
 * a Makefile is provided for compilation of `icbrstsplit.exe`. Macro for jean-zay is also provided. (Makefile read the `make.macro` default file, wich is a
link to macro_jean-zay).

## Usage:
A short usage message is obtained when using `icbrstsplit.exe` without parameters.

```
  usage : icbrstsplit.exe -r RST-root -l OUT-layout
       
      PURPOSE :
        Change the domain decomposition for ICB restart file
       
      ARGUMENTS :
       -r RST-root : give root name of input/output restart files
                     (omiting _<RANK>.nc)
       -l OUT-layout : layout file (produced by NEM0) for the target
                      domain decomposition.
       
      OPTIONS :
         None so far
       
      REQUIRED FILES :
         none 
       
      OUTPUT : 
       Use <RST-root>_split for output file names
      SEE ALSO :
```

