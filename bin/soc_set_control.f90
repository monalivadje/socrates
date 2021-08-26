! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps Control subroutine


module soc_set_control

Use, intrinsic :: ISO_C_BINDING
USE interface_core
use def_control,  only: StrCtrl, allocate_control
use def_spectrum, only: StrSpecData


implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_CONTROL'
contains

 subroutine set_controlwrap( l_set_defaults, &
  l_rayleigh, l_gas, l_continuum, l_cont_gen, l_orog, l_solvar, &
  l_rescale, l_ir_source_quad, l_mixing_ratio, &
  l_aerosol, l_aerosol_mode, l_aerosol_ccn, &
  l_tile, l_clear, &
  l_flux_up_band, l_flux_down_band, &
  l_flux_up_clear_band, l_flux_down_clear_band, &
  l_blue_flux_surf, &
  n_tile, n_cloud_layer, n_aer_mode, &
  isolir, i_cloud_representation, i_overlap, i_inhom, i_mcica_sampling, &
  i_st_water, i_cnv_water, i_st_ice, i_cnv_ice, i_drop_re ) BIND(C,name='set_control')


  !Use, intrinsic :: ISO_C_BINDING 
  ! use realtype_rd,  only: RealK
  use ereport_mod,  only: ereport
  use errormessagelength_mod, only: errormessagelength
  use missing_data_mod, only: imdi
  use rad_pcf, only: &
  ip_solar, ip_pifm80, ip_scatter_full, ip_infra_red, ip_elsasser, &
  ip_two_stream, ip_ir_gauss, ip_spherical_harmonic, ip_overlap_k_eqv_scl, &
  ip_cloud_off, ip_cloud_homogen, ip_cloud_ice_water, ip_cloud_conv_strat, &
  ip_cloud_csiw, ip_cloud_combine_homogen, ip_cloud_combine_ice_water, &
  ip_cloud_split_homogen, ip_cloud_split_ice_water, &
  ip_max_rand, ip_rand, ip_exponential_rand, ip_homogeneous, &
  ip_scaling, ip_mcica, ip_cairns, ip_cloud_ice_water, ip_cloud_mcica, &
  ip_no_scatter_abs, ip_no_scatter_ext, ip_solver_no_scat, &
  ip_solver_homogen_direct, ip_scatter_approx, ip_solver_mix_app_scat, &
  ip_solver_homogen_direct, ip_solver_mix_direct_hogan, ip_cloud_mix_max, &
  ip_cloud_part_corr, ip_cloud_mix_random, ip_solver_triple_app_scat, &
  ip_solver_triple_hogan, ip_cloud_triple, ip_cloud_part_corr_cnv, &
  ip_cloud_clear, ip_scale_ses2, ip_overlap_mix_ses2, ip_re_external, &
  i_normal, i_err_fatal
  use def_mcica, only: ip_mcica_optimal_sampling

  implicit none

  ! Control options:
  type(StrCtrl) :: control

  ! Spectral data:
  type(StrSpecData) :: spectrum

  Logical, intent(in), optional :: l_set_defaults, &
   l_rayleigh, l_gas, l_continuum, l_cont_gen, l_orog, l_solvar, &
   l_rescale, l_ir_source_quad, l_mixing_ratio, &
   l_aerosol, l_aerosol_mode, l_aerosol_ccn, &
   l_tile, l_clear, &
   l_flux_up_band, l_flux_down_band, &
   l_flux_up_clear_band, l_flux_down_clear_band, &
   l_blue_flux_surf

  integer(C_INT), intent(in), optional :: n_tile, n_cloud_layer, n_aer_mode

  integer(C_INT), intent(in), optional :: isolir, &
   i_cloud_representation, i_overlap, i_inhom, i_mcica_sampling, &
   i_st_water, i_cnv_water, i_st_ice, i_cnv_ice, i_drop_re

  Call set_control(control, spectrum, l_set_defaults, &
  l_rayleigh, l_gas, l_continuum, l_cont_gen, l_orog, l_solvar, &
  l_rescale, l_ir_source_quad, l_mixing_ratio, &
  l_aerosol, l_aerosol_mode, l_aerosol_ccn, &
  l_tile, l_clear, &
  l_flux_up_band, l_flux_down_band, &
  l_flux_up_clear_band, l_flux_down_clear_band, &
  l_blue_flux_surf, &
  n_tile, n_cloud_layer, n_aer_mode, &
  isolir, i_cloud_representation, i_overlap, i_inhom, i_mcica_sampling, &
  i_st_water, i_cnv_water, i_st_ice, i_cnv_ice, i_drop_re )
  
 end subroutine set_controlwrap

end module soc_set_control