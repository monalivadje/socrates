! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps dimension subroutine

module soc_set_dimen

Use, intrinsic :: ISO_C_BINDING
USE interface_core
use def_dimen,   only: StrDim
 use def_control, only: StrCtrl
 use def_mcica,   only: StrMcica, ip_mcica_full_sampling, &
  ip_mcica_single_sampling, ip_mcica_optimal_sampling

implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_DIMEN'
contains


subroutine set_dimenwrap(dimen, n_profile, n_layer, &
  n_channel, n_tile, n_cloud_layer, n_aer_mode, &
  n_direction, n_viewing_level, n_brdf_basis_fnc, n_brdf_trunc, &
  n_profile_aerosol_prsc, n_profile_cloud_prsc, &
  n_opt_level_aerosol_prsc, n_opt_level_cloud_prsc)BIND(C,name='set_dimen')
 
   
   use rad_pcf,     only: &
   ip_cloud_homogen, ip_cloud_ice_water, ip_cloud_conv_strat, ip_cloud_csiw, &
   ip_cloud_combine_homogen, ip_cloud_combine_ice_water, &
   ip_cloud_split_homogen, ip_cloud_split_ice_water, &
   ip_cloud_column_max, ip_solver_mix_app_scat, ip_solver_mix_direct, &
   ip_solver_mix_direct_hogan, ip_solver_triple_app_scat, ip_solver_triple, &
   ip_solver_triple_hogan, ip_two_stream, ip_spherical_harmonic, &
   ip_sph_mode_flux, ip_trunc_triangular, ip_trunc_azim_sym, &
   i_normal, i_err_fatal
   use ereport_mod, only: ereport
  use errormessagelength_mod, only: errormessagelength

   implicit none

  ! Dimensions:
  type(StrDim), intent(inout) :: dimen

  ! Control options:
  type(StrCtrl) :: control

  ! Mcica data:
  type(StrMcica) :: mcica_data

  integer (C_INT), intent(in) :: n_profile
  integer (C_INT), intent(in) :: n_layer
  integer (C_INT), intent(in), optional :: n_channel, n_tile
  integer (C_INT), intent(in), optional :: n_cloud_layer, n_aer_mode
  integer (C_INT), intent(in), optional :: n_direction, n_viewing_level
  integer (C_INT), intent(in), optional :: n_brdf_basis_fnc, n_brdf_trunc
  integer (C_INT), intent(in), optional :: n_profile_aerosol_prsc
  integer (C_INT), intent(in), optional :: n_profile_cloud_prsc
  integer (C_INT), intent(in), optional :: n_opt_level_aerosol_prsc
  integer (C_INT), intent(in), optional :: n_opt_level_cloud_prsc

  integer                      :: ierr = i_normal
  character (len=*), parameter :: RoutineName = 'SET_DIMEN'
  character (len=errormessagelength) :: cmessage

  Call set_dimen(dimen, control, n_profile, n_layer, mcica_data, &
   n_channel, n_tile, n_cloud_layer, n_aer_mode, &
   n_direction, n_viewing_level, n_brdf_basis_fnc, n_brdf_trunc, &
   n_profile_aerosol_prsc, n_profile_cloud_prsc, &
   n_opt_level_aerosol_prsc, n_opt_level_cloud_prsc) 
  
 end subroutine set_dimenwrap

end module soc_set_dimen
  