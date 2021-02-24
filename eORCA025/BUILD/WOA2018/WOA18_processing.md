# Processing WOA2018 data set before use with NEMO as initial conditions
## 1. Getting the data set from [NOAA site](https://www.nodc.noaa.gov/OC5/woa18/)
   * read the [detailed documentation](https://www.ncei.noaa.gov/data/oceans/woa/WOA18/DOC/woa18documentation.pdf) provided on the NOAA site.
   * The bash script [get_dta.sh](./get_dta.sh) can be use to download one set of a particular climatology

## 2. Simplifying WOA files
   * WOA files contains a lot variables in addition to the proper oceanic fields (temperature and salinity). Although fully necessary for
assessing the quality of the fields, they are not used directly  when preparing initial conditions. Therefore, for the sake of simplicity
we do extract only the on purpose variables. This is done through the [clean_files.sh](./clean_files.sh) bash script.

## 3. Creating full depth climatological files
   * WOA2018 files offer a monthly climatology for the upper ocean (top down to 1500m).
   * A quaterly climatology is available for the full  ocean depth (top down to 5500m)
   * A specific program (f90) has been developped ([combine_monthly_w18](https://github.com/molines/JMMTOOLS/blob/master/DATA_TOOLS/LEVITUS-WOA/combine_monthly_w18.f90) )
for creating a monthly full depth climatology by combining monthly and quaterly fields.

## 4. Converting in situ temperature and relative salinity according to the equation of state used in NEMO.
   * WOA2018 files give the ocean temperature as *in-situ* temperatures, and salinity as relative salinity.
   * NEMO is using either EOS80 equation of state (requiring potential temperature \{theta}0) or TEOS10 equation of state (using conservative 
temperature 'CT' and absolute salinity 'SA')
   * Conversion to \theta-0 can be performed by the program [t_to_theta](https://github.com/molines/JMMTOOLS/blob/master/DATA_TOOLS/LEVITUS-WOA/t_to_theta.f90).
   * conversion to CT and SA can be performed by ....
   * Note that the conversion program are on a different [githup repository](https://github.com/molines/JMMTOOLS), and specifically in the sub-directory DATA_TOOLS.

