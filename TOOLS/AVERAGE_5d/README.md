# 5 day averaging of yearly 1d output

## size of file

```
(base) cli [rcli002@jean-zay3: 1979]$ ls -lSrh *m01d01*
-rw-r--r-- 1 rcli002 cli  27K Aug  8 05:42 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_fwbscalar.nc
-rw-r--r-- 1 rcli002 cli  14M Aug  8 05:47 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_ICBT.nc
-rw-r--r-- 1 rcli002 cli  17M Aug  8 05:45 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_gridUsurf.nc
-rw-r--r-- 1 rcli002 cli  17M Aug  8 05:42 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_gridVsurf.nc
-rw-r--r-- 1 rcli002 cli  35M Aug  8 05:46 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_gridTsurf.nc
-rw-r--r-- 1 rcli002 cli  38M Aug  8 05:42 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_icemod.nc
-rw-r--r-- 1 rcli002 cli  65M Aug  8 05:47 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_flxT.nc
-rw-r--r-- 1 rcli002 cli 324M Aug  8 05:47 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_gridU.nc
-rw-r--r-- 1 rcli002 cli 326M Aug  8 05:45 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_gridV.nc
-rw-r--r-- 1 rcli002 cli 447M Aug  8 05:48 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_gridT.nc
-rw-r--r-- 1 rcli002 cli 564M Aug  8 05:47 eORCA025.L75-IMHOTEP.ES.005_y1979m01d01.1d_gridW.nc
```

##  3D files : do not keep daily
  * gridU

    ```
    float depthu(depthu) ;
    float depthu_bounds(depthu, axis_nbounds) ;
    float e3u(time_counter, depthu, y, x) ;
    float vozocrtx(time_counter, depthu, y, x) ;
    float sozotaux(time_counter, y, x) ;
    ```

  * gridV

    ```
    float depthv(depthv) ;
    float depthv_bounds(depthv, axis_nbounds) ;
    float e3v(time_counter, depthv, y, x) ;
    float vomecrty(time_counter, depthv, y, x) ;
    float sometauy(time_counter, y, x) ;
    ```

  * gridT

    ```
    float deptht(deptht) ;
    float deptht_bounds(deptht, axis_nbounds) ;
    float e3t(time_counter, deptht, y, x) ;
    float votemper(time_counter, deptht, y, x) ;
    float vosaline(time_counter, deptht, y, x) ;
    ```

  * gridW : NO VVL ( interface value).

    ```
    float depthw(depthw) ;
    float depthw_bounds(depthw, axis_nbounds) ;
    float vovecrtz(time_counter, depthw, y, x) ;
    float voavt(time_counter, depthw, y, x) ;
    float avtiwm(time_counter, depthw, y, x) ;
    ```

## 2D files: keep daily + 5 day average (no VVL dependent)
  * ICBT

    ```
    float berg_melt(time_counter, y, x) ;
    float berg_buoy_melt(time_counter, y, x) ;
    float berg_eros_melt(time_counter, y, x) ;
    float berg_conv_melt(time_counter, y, x) ;
    ```

  * gridUsurf

    ```
    float vozocrtx(time_counter, y, x) ;
    ```

  * gridVsurf

    ```
    float vomecrty(time_counter, y, x) ;
    ```

  * gridTsurf

    ```
    float sosstsst(time_counter, y, x) ;
    float sosaline(time_counter, y, x) ;
    float sossheig(time_counter, y, x) ;
    float botpres(time_counter, y, x) ;
    float sosshths(time_counter, y, x) ;
    float sosshhas(time_counter, y, x) ;
    float somxl010(time_counter, y, x) ;
    ```

  * icemod.nc  5d average only

    ```
    float simsk(time_counter, y, x) ;
    float simsk15(time_counter, y, x) ;
    float snvolu(time_counter, y, x) ;
    float snthic(time_counter, y, x) ;
    float sithic(time_counter, y, x) ;
    float sivolu(time_counter, y, x) ;
    float siconc(time_counter, y, x) ;
    float sisali(time_counter, y, x) ;
    float sivelu(time_counter, y, x) ;
    float sivelv(time_counter, y, x) ;
    float sivelo(time_counter, y, x) ;
    float utau_ai(time_counter, y, x) ;
    float vtau_ai(time_counter, y, x) ;
    float utau_oi(time_counter, y, x) ;
    float vtau_oi(time_counter, y, x) ;
    float sidive(time_counter, y, x) ;
    float sishea(time_counter, y, x) ;
    float sistre(time_counter, y, x) ;
    float qt_oce_ai(time_counter, y, x) ;
    float qt_atm_oi(time_counter, y, x) ;
    float qtr_ice_top(time_counter, y, x) ;
    float qemp_ice(time_counter, y, x) ;
    float albedo(time_counter, y, x) ;
    float hfxcndbot(time_counter, y, x) ;
    float hfxsensib(time_counter, y, x) ;
    float sfxice(time_counter, y, x) ;
    float vfxice(time_counter, y, x) ;
    float vfxsnw(time_counter, y, x) ;
    float vfxdyn(time_counter, y, x) ;
    ```
  * flxT.nc

    ```
    float somixhgt(time_counter, y, x) ;
    float sohefldo(time_counter, y, x) ;
    float soshfldo(time_counter, y, x) ;
    float qns_oce(time_counter, y, x) ;
    float qns(time_counter, y, x) ;
    float solhflup(time_counter, y, x) ;
    float solwfldo(time_counter, y, x) ;
    float sosbhfup(time_counter, y, x) ;
    float sowaflup(time_counter, y, x) ;
    float sosfldow(time_counter, y, x) ;
    float wdmp(time_counter, y, x) ;
    float sohumspe(time_counter, y, x) ;
    float sotemair(time_counter, y, x) ;
    float sowapre(time_counter, y, x) ;
    float fwfisf(time_counter, y, x) ;
    float mqnet(time_counter, y, x) ;
    float msfx(time_counter, y, x) ;
    float mqsr(time_counter, y, x) ;
    float sowinsp(time_counter, y, x) ;
    float sornf(time_counter, y, x) ;
    ```

