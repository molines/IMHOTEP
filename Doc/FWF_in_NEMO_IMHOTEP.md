# Fresh Water Forcing in NEMO, IMHOTEP configurations.

## Context:
IMHOTEP has a central focus based on the frewh water forcing, and in particular on continental fresh water fluxes. Therefore, this
report aims at looking in details how this fresh water forcing is implemented in NEMO.

## Ingredients of the fresh water forcing (FWF):
For IMHOTEP we choose the following parameters that have an influence on FWF. We first list the choices we have made, following the 
chronological order of the NEMO calls.

### Choice of NCAR bulk formulae:
In former version, these formulae where known as the CORE bulk formulae (Large & Yeager 20xx).  When called, they provided  the term `emp` 
(Evaporation - Precipitations), in kg/m2/s, net from any other contributions. Note that both Evaporation (computed) and precipitation 
(read from external forcing files), can be multiplied by a scaling coefficient, passed to NEMO via the namelist. In our case both scaling
coefficient are set to 1. 

###  Choice of SI3 sea ice model:

###  Choice of explicit Iceberg representation:

### Choice of iceshelf melting parameterization:

### Choice of continental river runoff:

### Choice of SSS restoring:

### Choice of FW budget correction:

