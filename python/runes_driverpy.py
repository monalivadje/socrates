import os
import numpy as np
from constants_runes import grav_acc, r_gas_dry, cp_air_dry, pi, seconds_per_day, co2_mix_ratio, ch4_mix_ratio ,n2o_mix_ratio, o2_mix_ratio, n_profile, n_layer, t_ground, solar_irrad, grey_albedo_sw, grey_albedo_lw 
import cffi
from cffi import FFI
from os.path import abspath

runes_library_path = abspath('./libicore.a')
runeslib = os.path.abspath(runes_library_path)
desired_compiler = 'gcc'

ffibuilder = cffi.FFI()
ffibuilder.cdef("void runes_wrapper( int n_profile , int n_layer , char spectrum_name , double i_source , float *p_layer, float *t_layer , float *mass , float *density , float *layer_heat_capacity , float *h2o , float *o3 , float co2_mix_ratio , float n2o_mix_ratio , float ch4_mix_ratio , float o2_mix_ratio , float cos_zenith_angle ,float solar_irrad , bool l_grey_albedo, float grey_albedo , bool l_rayleigh , bool l_invert ,float *heating_rate, float *flux_up, float *flux_down);", override=True)  
ffibuilder.cdef("void set_spectrum( char spectrum_name , bool l_all_gases);", override=True)

ffibuilder.set_source("runesdri", r''' #include "runes.h" ''', library_dirs=[os.getcwd()], include_dirs=[os.getcwd()],extra_link_args=[os.path.abspath(runes_library_path), '-lgfortran',])
ffibuilder.compile(verbose=True)
