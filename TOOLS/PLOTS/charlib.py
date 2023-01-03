class Color(object):
     " This class corresponds to color plot layer"
     def __init__(self, clrdata ='None', clrvar='None', clrmin=-9999, clrmax=9999,  \
                        clrlnam ='None', offset=0     , scalef=1    , unit='None',  \
                        tick=None      , lmsk=False ):
         self.clrdata = clrdata
         self.clrvar  = clrvar
         self.clrmin  = clrmin
         self.clrmax  = clrmax
         self.clrlnam = clrlnam
         self.offset  = offset
         self.scalef  = scalef
         self.unit    = unit
         self.tick    = tick
         self.lmsk    = lmsk
 
   
     def show(self):
         " Show the attributes of a color object "
         print " Color :"
         print "    filename : ", self.clrdata
         print "    variable : ", self.clrvar
         print "        vmin : ", self.clrmin
         print "        vmax : ", self.clrmax
         print "     clrlnam : ", self.clrlnam
         print "      offset : ", self.offset
         print "      scalef : ", self.scalef
         print "        unit : ", self.unit
         print "        tick : ", self.tick
         print "        lmsk : ", self.lmsk

def colorinit(color):
        # predifined variables ... for each predefined variables (according to their name in netcdf data file)
        #                          some values are defined ... lmsk is set to true for ocean variable
        #                          note the vmin and vmax can be overwritten using command line option -vmin <value> -vmax <value>
        cv_in=color.clrvar
        if cv_in == 'SST':
            cname  = 'Surface temperature '
            vmin   = 14
            vmax   = 30
            offset = -273.
            scalef = 1.
            unit   = 'DegC'
            tick   = 2
            lmsk   = False
        elif cv_in == 'WSPD10':
            cname  = '10m wind speed'
            vmin   = 0
            vmax   = 25
            offset = 0
            scalef = 1.
            unit   = 'm/s'
            tick   = 5
            lmsk   = True
        elif cv_in == 'HFX':
            cname  = 'Sensible Heat Flux'
            vmin   = -400
            vmax   = 400
            offset = 0
            scalef = 1.
            unit   = 'W/m2'
            tick   = 100
            lmsk   = False
        elif cv_in == 'LH':
            cname  = 'Latent Heat Flux'
            vmin   = -400
            vmax   = 400
            offset = 0
            scalef = 1.
            unit   = 'W/m2'
            tick   = 100
            lmsk   = False

        elif cv_in == 'tos':
            cname  = 'Sea Surface temperature '
            vmin   = 0
            vmax   = 30
            offset = 0.
            scalef = 1.
            unit   = 'DegC'
            tick   = 5
            lmsk   = True

        elif cv_in == 'QFX':
            cname  = 'Evaporation '
            vmin   = 0
            vmax   = 10
            offset = 0
            scalef = 86400.
            unit   = 'mm/day'
            tick   = 1
            lmsk   = False

        elif cv_in == 'QFX_SEA':
            cname  = 'Evaporation '
            vmin   = 0
            vmax   = 10
            offset = 0
            scalef = 86400.
            unit   = 'mm/day'
            tick   = 1
            lmsk   = False

        elif cv_in == 'sos':
            cname  = 'Sea Surface Salinity '
            vmin   = 35.5
            vmax   = 37.5
            offset = 0
            scalef = 1
            unit   = '--'
            tick   = 0.4
            lmsk   = True

        elif cv_in == 'sovitmod':
            cname  ='Sea Surface Velocity '
            vmin   = 0.
            vmax   = 1.
            offset = 0
            scalef = 1
            unit   = 'm/s'
            tick   = 0.1
            lmsk   = True

        elif cv_in == 'sosstsst':
            cname  = 'Sea Surface Temperature '
            vmin   = -2.0
            vmax   = 32.0
            offset = 0
            scalef = 1
            unit   = 'DegC'
            tick   = 2
            lmsk   = True

        elif cv_in == 'sosaline':
            cname  = 'Sea Surface Salinity '
            vmin   = 32
            vmax   = 35
            offset = 0
            scalef = 1
            unit   = 'PSU'
            tick   = 0.5
            lmsk   = True

        elif cv_in == 'votemper':
            cname  = 'Conservative Temperature'
            vmin   = 0
            vmax   = 30
            offset = 0
            scalef = 1
            unit   = 'DegC'
            tick   = 1
            lmsk   = True

        elif cv_in == 'vosaline':
            cname  = 'Absolute Salinity'
            vmin   = 32
            vmax   = 35
            offset = 0
            scalef = 1
            unit   = 'g/kg'
            tick   = 0.2
            lmsk   = True

        elif cv_in == 'voeke':
            cname  = 'Eddy Kinetic Energy'
            vmin   = 0
            vmax   = 2750.
            offset = 0
            scalef = 10000
            unit   = 'cm2/s2'
            tick   = 250
            lmsk   = True
        
        elif cv_in == 'sobarstf':
            cname = 'Barotropic Stream Function'
            vmin   = -100
            vmax   = 200
            offset = 0
            scalef = 1.0e-6
            unit   = 'Sv'
            tick   = 10
            lmsk   = True

        elif cv_in == 'sossheig':
            cname  = 'Sea Surface Height'
            vmin   = -2
            vmax   = 2
            offset = 0
            scalef = 1
            unit   = 'm'
            tick   = 0.2
            lmsk   = True

        elif cv_in == 'somxl010':
            cname  = 'Mixed Layer Depth Rho 0.01 Crit'
            vmin   = 0
            vmax   = 2500
            offset = 0
            scalef = 1
            unit   = 'm'
            tick   = 500
            lmsk   = True

        elif cv_in == 'somxl030':
            cname  = 'Mixed Layer Depth Rho 0.03 Crit'
            vmin   = 0
            vmax   = 2500
            offset = 0
            scalef = 1
            unit   = 'm'
            tick   = 500
            lmsk   = True

        elif cv_in == 'somxlt02':
            cname  = 'Mixed Layer Depth Temp 0.2 Crit'
            vmin   = 0
            vmax   = 2500
            offset = 0
            scalef = 1
            unit   = 'm'
            tick   = 500
            lmsk   = True

        elif cv_in == 'siconc':
            cname  = 'Sea ice concentration'
            vmin   = 0.
            vmax   = 1.
            offset = 0
            scalef = 1
            unit   = ''
            tick   = 0.1
            lmsk   = False
        elif cv_in == 'berg_melt':
            cname  = 'Iceberg melting'
            vmin   = 0.
            vmax   = 1.
            offset = 0
            scalef = 86400
            unit   = 'mm/day'
            tick   = 0.5
            lmsk   = False
        elif cv_in == 'sowaflup':
            cname  = 'Fresh Water Flux up'
            vmin   = -10
            vmax   = 10
            offset = 0
            scalef = 86400
            unit   = 'mm/day'
            tick   = 1
            lmsk   = True
        elif cv_in == 'sowafld':
            cname  = 'Fresh Water Flux damping'
            vmin   = -10
            vmax   = 10
            offset = 0
            scalef = 86400
            unit   = 'mm/day'
            tick   = 1
            lmsk   = True
        elif cv_in == 'sowapre':
            cname  = 'Precip'
            vmin   = 0
            vmax   = 10
            offset = 0
            scalef = 86400
            unit   = 'mm/day'
            tick   = 1
            lmsk   = True
        elif cv_in == 'solhflup':
            cname  = 'Evaporation'
            vmin   = 0
            vmax   = 10
            offset = 0
            scalef = -86400/2.5e6
            unit   = 'mm/day'
            tick   = 1
            lmsk   = True
        elif cv_in == 'vfxice':
            cname  = 'ice-ocean fwf ice melt/growth'
            vmin   = -10
            vmax   = 10
            offset = 0
            scalef = 86400
            unit   = 'mm/day'
            tick   = 1
            lmsk   = True
        elif cv_in == 'vfxsnw':
            cname  = 'ice-ocean fwf snw melt/growth'
            vmin   = -10
            vmax   = 10
            offset = 0
            scalef = 86400
            unit   = 'mm/day'
            tick   = 1
            lmsk   = True

        else:
            print 'ERROR : variable ',cv_in, ' not yet supported.' 
            quit()
        
        color.clrlnam = cname
        color.clrmin  = vmin
        color.clrmax  = vmax
        color.offset  = offset
        color.scalef  = scalef
        color.unit    = unit
        color.tick    = tick
        color.lmsk    = lmsk

