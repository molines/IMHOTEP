# Building domain_cfg file
## 1. Context
Since NEMO4, all the parameters concerning the computational grid of a configuration (horizontal **and** vertical) are read from a `domain_cfg`file. In previous
NEMO version, only the horizontal part was externalized (in coordinates.nc file). The bathymetry was read from a file, and the vertical grid was built during
NEMO initialisation, taking relevant information from the namelist.  In NEMO4, the construction of the vertical grid is done off-line with the help of a NEMO tool
called DOMAIN_cfg, taking the grid information from a NEMO_3.6 like namelist. In particular, the coefficient for the vertical grid (in namdom) should be carefully set,
including parameters for the partial cells definition.  The drawback of this new procedure is that any change in bathymetry implies a rebuild of the domain_cfg file.
The advantage is in the simplification of the NEMO code where configuration related information is read in an input file.  Therefore, this file hold key information
for a configuration, and the building of this file must be carefully documented, for tracability.  A DRAKKAR improvement, is the inclusion of the used namelist, together with the name of all input files (bathymetry, coordinates) in the domain_cfg file.

## 2. Step by step
  1. Getting the DOMAIN_cfg tool ready

  ```
  cd $UDIR/CONFIG_eORCA025.L75/eORCA025.L75-IMHOTEP00/
  dcm_mktools -n DOMAIN_cfg -m X64_JEANZAY_jm  -c eORCA025.L75-IMHOTEP00
  ```
 
 The tool is ready to be used in `$WORKDIR/WORCA025.L75-IMHOTEP00/tools/DOMAIN_cfg` directory.
  
  2. Work in `$WORKDIR/eORCA025.L75/eORCA025.L75-I/DOMAIN_cfg_build`:

  ```
  mkdir -p $WORKDIR/eORCA025.L75/eORCA025.L75-I/DOMAIN_cfg_build
  cd $WORKDIR/eORCA025.L75/eORCA025.L75-I/DOMAIN_cfg_build
  ln -sf $WORKDIR/WORCA025.L75-IMHOTEP00/tools/DOMAIN_cfg/*.exe ./
  ```
   
  Copy the namelists and scripts required by the tool, and get bathy and coordinated from the opendap.

  ```
  cp $DEVGIT/IMHOTEP/eORCA025/BUILD/DOMAIN_cfg/* ./
  chmod 755 get_bat_coord.sh
  ./get_bat_coord.sh
  ```

> update 09/04/2021: In the final process original bathymetry has been modified around Greenland. New bathymetry is `eORCA025_bathymetry_b0.2_closed_seas_greenland.nc`

 3. Fills closed seas in the original bathymetry: ( this script is valid only for eORCA025)

   ```
   ./close_seas.sh 
   ```

> update 09/04/2021: This step is already done for `eORCA025_bathymetry_b0.2_closed_seas_greenland.nc`.

 4. Edit the `jobcfg` file to the machine you are using
  This concerns the header of the batch script, and the number of core you are using.
  Note that the `nammpp` block of the `namelist_cfg` should be coherent with the number of core asked for,
adjusting `jpni, jpnj` and `jpnij`.
> Tip : it is a good practice at this step to use jpni=1 in order to use zonal stripes. The output of the
> tool will be a bunch of jpnj files without missing land proc, hence the rebuild file will be continuous.  

   Submit the batch.  
    
 5. Rebuild the domain_cfg file in a single piece using rebuild_nemo tool.

   ```
   rebuild_nemo -d 1 domain_cfg <jpnj>
   ```

 6. Document the new domain_cfg file :

   ```
   ./dcmtk_dom_doc.exe  -b eORCA025_bathymetry_b0.2_closed_seas_greenland.nc -c eORCA025_coord_c3.0.nc -n namelist_cfg -d domain_cfg.nc
   ```

> update 09/04/2021: name of bathy file

 7. Move the final file to the `eORCA025.L75-I` directory:

   ```
   mv domain_cfg.nc ../eORCA025.L75_domain_cfg_closed_seas_greenland.nc
   ```

> update 09/04/2021: domain file name changed

 8. **YOU ARE DONE !**

