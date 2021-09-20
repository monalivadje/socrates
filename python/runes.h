#include <stdbool.h>
void runes_wrapper( int n_profile , int n_layer , char spectrum_name , double i_source , float *p_layer, float *t_layer, float *mass , float *density , float *layer_heat_capacity , float  *h2o , float *o3 , float co2_mix_ratio ,  float n2o_mix_ratio , float ch4_mix_ratio , float o2_mix_ratio , float cos_zenith_angle , float solar_irrad , bool l_grey_albedo, float grey_albedo , bool l_rayleigh , bool l_invert ,float *heating_rate, float *flux_up,  float *flux_down);
void set_spectrum( char spectrum_name , char spectral_file , bool l_all_gases);
