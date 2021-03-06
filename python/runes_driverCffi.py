
# python interface to fortran module runes_driver using cffi

import sys
import numpy as np
from constants_runes import grav_acc,r_gas_dry,cp_air_dry,pi,seconds_per_day , co2_mix_ratio , ch4_mix_ratio ,n2o_mix_ratio, o2_mix_ratio ,cos_zenith_angle ,n_profile, n_layer, t_ground,solar_irrad,grey_albedo_sw ,grey_albedo_lw 
import cffi
from cffi import FFI
import runesdri

ffi = cffi.FFI()
def numpy_pointer(numpy_array,ffi):
    assert numpy_array.flags['F_CONTIGUOUS'], \
        "array is not contiguous in memory (Fortran order)"
    return ffi.cast("float*", numpy_array.__array_interface__['data'][0])

def main():

 ffibuilder = cffi.FFI()

#spectrum_name = input('Enter spectrum name:\n')
isource = input(" enter i_source value: ")
i_source = int(isource)

  p_layer = np.asfortranarray([[0.337000E+01, 0.509050E+02, 0.135550E+03, 0.254500E+03],[0.492500E+03,0.986000E+03, 0.204500E+04, 0.299500E+04],[0.349000E+04, 0.406500E+04, 0.473500E+04, 0.552500E+04],[0.645000E+04, 0.753500E+04, 0.881000E+04, 0.103000E+05],[0.120500E+05, 0.141500E+05, 0.166000E+05, 0.194000E+05],[0.226000E+05, 0.262000E+05, 0.302500E+05,0.348000E+05],[0.399000E+05, 0.456500E+05, 0.520500E+05, 0.591000E+05],[0.669000E+05, 0.756000E+05, 0.852000E+05, 0.957500+05]], dtype=object)
     

  t_layer = np.asfortranarray([[0.216982E+03, 0.262328E+03, 0.272545E+03, 0.263059E+03],[0.250428E+03, 0.238550E+03, 0.228094E+03, 0.223481E+03],[0.222481E+03, 0.220962E+03, 0.219481E+03, 0.218481E+03],[0.217481E+03, 0.216481E+03, 0.216000E+03, 0.216000E+03],[0.216000E+03, 0.216000E+03, 0.216000E+03,0.219116E+03],[0.225632E+03, 0.232109E+03, 0.238624E+03,0.245104E+03], [0.251619E+03, 0.258100E+03, 0.264097E+03, 0.270094E+03],[0.276092E+03, 0.282091E+03, 0.287573E+03, 0.292058E+03]], dtype=object) 
 

  p_level = np.asfortranarray([[0.300000E-01, 0.671000E+01, 0.951000E+02, 0.176000E+03],[0.333000E+03, 0.652000E+03, 0.132000E+04, 0.277000E+04],[0.322000E+04, 0.376000E+04, 0.437000E+04, 0.510000E+04],[0.595000E+04, 0.695000E+04, 0.812000E+04, 0.950000E+04],[0.111000E+05, 0.130000E+05, 0.153000E+05, 0.179000E+05],[0.209000E+05, 0.243000E+05, 0.281000E+05, 0.324000E+05],[0.372000E+05, 0.426000E+05, 0.487000E+05, 0.554000E+05],[0.628000E+05, 0.710000E+05, 0.802000E+05, 0.902000E+05], [0.101300E+06]], dtype=object)
  

  t_level = np.asfortranarray([[0.210000E+03, 0.218000E+03, 0.276000E+03, 0.270000E+03],[0.258000E+03, 0.245000E+03, 0.234000E+03, 0.224000E+03],[0.223000E+03, 0.222000E+03, 0.220000E+03, 0.219000E+03],[0.218000E+03, 0.217000E+03, 0.216000E+03, 0.216000E+03],[0.216000E+03, 0.216000E+03, 0.216000E+03, 0.216000E+03],[0.222000E+03, 0.229000E+03, 0.235000E+03, 0.242000E+03],[0.248000E+03, 0.255000E+03, 0.261000E+03, 0.267000E+03],[0.273000E+03, 0.279000E+03, 0.285000E+03, 0.290000E+03], [0.294000E+03]], dtype=object)     


  h2o = np.asfortranarray([[0.399688E-05, 0.399530E-05, 0.399851E-05, 0.399700E-05],[0.399963E-05,  0.400241E-05, 0.400722E-05, 0.400994E-05],[0.400705E-05, 0.400353E-05, 0.399929E-05, 0.399791E-05],[0.399939E-05, 0.400000E-05, 0.400000E-05, 0.400058E-05], [0.400152E-05, 0.402072E-05, 0.485647E-05, 0.109264E-04],[0.349482E-04, 0.974304E-04, 0.199405E-03, 0.321272E-03], [0.509681E-03, 0.777969E-03, 0.114820E-02, 0.182544E-02],[0.305008E-02, 0.485372E-02, 0.722366E-02, 0.101064E-01]], dtype=object) 


  o3 = np.asfortranarray([[0.606562E-06, 0.252165E-05, 0.469047E-05, 0.748127E-05],[ 0.957770E-05, 0.100812E-04, 0.814088E-05, 0.664711E-05],[0.603987E-05, 0.546986E-05, 0.480064E-05, 0.397211E-05],[0.319003E-05, 0.246208E-05,0.181795E-05, 0.135296E-05],[0.102925E-05, 0.808670E-06, 0.612577E-06, 0.434212E-06],[0.328720E-06, 0.252055E-06, 0.198937E-06,0.166297E-06], [0.139094E-06, 0.116418E-06, 0.981116E-07, 0.850660E-07],[0.743462E-07, 0.649675E-07, 0.577062E-07,  0.520021E-07]], dtype=object) 

  l_grey_albedo= bool(input(" enter l_grey_albedo value: \n"))
  l_rayleigh = bool(input(" enter l_rayleigh value: \n"))
  l_invert = bool(input(" enter l_invert value: \n"))
  shape = p_layer.shape
  mass = np.empty(shape, dtype=float, order="F")
  density = np.empty(shape, dtype=float, order="F")
  layer_heat_capacity = np.empty(shape, dtype=float, order="F")
 
  sw_heating_rate = np.empty((n_profile, n_layer), dtype=float, order="F")
  lw_heating_rate = np.empty((n_profile, n_layer), dtype=float, order="F")
  sw_flux_up = np.empty((n_profile, n_layer+1), dtype=float, order="F")
  sw_flux_down = np.empty((n_profile, n_layer+1), dtype=float, order="F")
  lw_flux_up = np.empty((n_profile, n_layer+1), dtype=float, order="F")
  lw_flux_down = np.empty((n_profile, n_layer+1), dtype=float, order="F")

  print("sw_heating_rate:",sw_heating_rate)
  print("lw_heating_rate:",lw_heating_rate)
  print("sw_flux_up:",sw_flux_up)
  print("sw_flux_down:",sw_flux_down)

  runesdri.lib.runes_wrapper(n_profile ,n_layer ,i_source , numpy_pointer(p_layer, ffi), numpy_pointer(t_layer,ffi), numpy_pointer(mass,ffi) , numpy_pointer(density,ffi) ,numpy_pointer(layer_heat_capacity,ffi) ,numpy_pointer(h2o,ffi), numpy_pointer(o3,ffi), co2_mix_ratio, n2o_mix_ratio , ch4_mix_ratio , o2_mix_ratio , cos_zenith_angle , solar_irrad , l_grey_albedo, grey_albedo, l_rayleigh ,l_invert, numpy_pointer(sw_heating_rate,ffi), numpy_pointer(sw_flux_up,ffi), numpy_pointer(sw_flux_down,ffi)) 

   print("in python after call to runes_wrap trying to display heating rate, flux_up , flux_down to SW")
   print(sw_heating_rate) 
   print(sw_flux_up) 
   print(sw_flux_down)

  runesdri.lib.runes_wrapper(n_profile ,n_layer , i_source , numpy_pointer(p_layer, ffi), numpy_pointer(t_layer,ffi) , numpy_pointer(mass,ffi) , numpy_pointer(density,ffi) ,numpy_pointer(layer_heat_capacity,ffi) , numpy_pointer(h2o,ffi), numpy_pointer(o3,ffi), co2_mix_ratio , n2o_mix_ratio , ch4_mix_ratio ,o2_mix_ratio , cos_zenith_angle , solar_irrad , l_grey_albedo, grey_albedo, l_rayleigh , l_invert, numpy_pointer(lw_heating_rate,ffi), numpy_pointer(lw_flux_up,ffi),numpy_pointer(lw_flux_down,ffi))


   print("in python after call to runes_wrap trying to display heating rate, flux_up , flux_down to LW")
   print(lw_heating_rate) 
   print(lw_flux_up) 
   print(lw_flux_down)
   
if __name__ == "__main__":
   ffibuilder.compile(verbose=True)
    main()
    
