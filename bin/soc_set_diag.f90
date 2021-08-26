! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps diagnostic subroutine

module soc_set_diag

Use, intrinsic :: ISO_C_BINDING
USE interface_core
use def_control,  only: StrCtrl
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
 use def_atm,      only: StrAtm
 use def_cld,      only: StrCld
 use def_mcica,    only: StrMcica
 use def_aer,      only: StrAer
 use def_bound,    only: StrBound
 use def_out,      only: StrOut


implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_DIAG'
contains


 subroutine set_diagwrap( n_profile, n_layer, n_tile, &
  layer_heat_capacity, layer_heat_capacity_1d, l_invert, &
  flux_direct, flux_down, flux_up, heating_rate, &
  flux_up_tile, flux_up_blue_tile, flux_direct_blue_surf, flux_down_blue_surf, &
  flux_direct_1d, flux_down_1d, flux_up_1d, heating_rate_1d, &
  flux_up_tile_1d, flux_up_blue_tile_1d, &
  total_cloud_cover, total_cloud_fraction, total_cloud_fraction_1d, &
  liq_frac_diag, ice_frac_diag, &
  liq_conv_frac_diag, ice_conv_frac_diag, &
  liq_incloud_mmr_diag, ice_incloud_mmr_diag, &
  liq_inconv_mmr_diag, ice_inconv_mmr_diag, &
  liq_dim_diag, ice_dim_diag, &
  liq_conv_dim_diag, ice_conv_dim_diag, &
  liq_frac_diag_1d, ice_frac_diag_1d, &
  liq_conv_frac_diag_1d, ice_conv_frac_diag_1d, &
  liq_incloud_mmr_diag_1d, ice_incloud_mmr_diag_1d, &
  liq_inconv_mmr_diag_1d, ice_inconv_mmr_diag_1d, &
  liq_dim_diag_1d, ice_dim_diag_1d, &
  liq_conv_dim_diag_1d, ice_conv_dim_diag_1d) BIND(C,name='set_diag')


 ! Use, intrinsic :: ISO_C_BINDING 
 ! use def_control,  only: StrCtrl
 ! use def_dimen,    only: StrDim
 ! use def_spectrum, only: StrSpecData
 ! use def_atm,      only: StrAtm
 ! use def_cld,      only: StrCld
 ! use def_mcica,    only: StrMcica
 ! use def_aer,      only: StrAer
 ! use def_bound,    only: StrBound
 ! use def_out,      only: StrOut

 ! use realtype_rd, only: RealK
 ! use ereport_mod,  only: ereport
  use errormessagelength_mod, only: errormessagelength
  use rad_pcf, only: i_normal, i_err_fatal, ip_cloud_off, ip_mcica, &
                   ip_clcmp_st_water, ip_clcmp_st_ice, &
                   ip_clcmp_cnv_water, ip_clcmp_cnv_ice

  implicit none

  ! Control options:
  type(StrCtrl) :: control

  ! Dimensions:
  type(StrDim) :: dimen

  ! Spectral data:
  type (StrSpecData) :: spectrum

  ! Atmospheric properties:
  type(StrAtm) :: atm

  ! Cloud properties:
  type(StrCld) :: cld

  ! MCICA data:
  type(StrMcica) :: mcica_data

  ! Aerosol properties:
  type(StrAer) :: aer

  ! Boundary conditions:
  type(StrBound) :: bound

  ! Output fields from core radiation code:
  type(StrOut) :: radout

  integer (C_INT), intent(in) :: n_profile
  !   Number of columns to operate on
  integer (C_INT), intent(in) :: n_layer
  !   Number of layers for radiation
  integer (C_INT), intent(in), optional :: n_tile
  !   Number of surface tiles

  real(C_DOUBLE), intent(in), optional :: layer_heat_capacity(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: layer_heat_capacity_1d(n_layer)
  !   Heat capacity of layer

  Logical, intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical


  ! Output fields:
  real(C_DOUBLE), intent(out), optional :: flux_direct(n_profile, 0:n_layer)
  real(C_DOUBLE), intent(out), optional :: flux_direct_1d(0:n_layer)
  !   Direct (unscattered) downwards flux (Wm-2)
  real(C_DOUBLE), intent(out), optional :: flux_down(n_profile, 0:n_layer)
  real(C_DOUBLE), intent(out), optional :: flux_down_1d(0:n_layer)
  !   Downwards flux (Wm-2)
  real(C_DOUBLE), intent(out), optional :: flux_up(n_profile, 0:n_layer)
  real(C_DOUBLE), intent(out), optional :: flux_up_1d(0:n_layer)
  !   Upwards flux (Wm-2)
  real(C_DOUBLE), intent(out), optional :: heating_rate(n_profile, n_layer)
  real(C_DOUBLE), intent(out), optional :: heating_rate_1d(n_layer)
  !   Heating rate (Ks-1)
  real(C_DOUBLE), intent(out), optional :: flux_up_tile(:, :) ! (n_profile, n_tile)
  real(C_DOUBLE), intent(out), optional :: flux_up_tile_1d(:) ! (n_tile)
  !   Upwards flux on tiles (Wm-2)
  real(C_DOUBLE), intent(out), optional :: flux_up_blue_tile(:, :)
  real(C_DOUBLE), intent(out), optional :: flux_up_blue_tile_1d(:)
  !   Upwards blue flux on tiles (Wm-2)
  real(C_DOUBLE), intent(out), optional :: flux_direct_blue_surf(n_profile)
  !   Direct blue flux at the surface
  real(C_DOUBLE), intent(out), optional :: flux_down_blue_surf(n_profile)
  !   Total downward blue flux at the surface
  real(C_DOUBLE), intent(out), optional :: total_cloud_cover(n_profile)
  !   Total cloud cover
  real(C_DOUBLE), intent(out), optional :: total_cloud_fraction(n_profile, n_layer)
  real(C_DOUBLE), intent(out), optional :: total_cloud_fraction_1d(n_layer)
  !   Total cloud fraction in layers
  real(C_DOUBLE), intent(out), dimension(n_profile, n_layer), optional :: &
   liq_frac_diag, ice_frac_diag, &
   liq_conv_frac_diag, ice_conv_frac_diag, &
   liq_incloud_mmr_diag, ice_incloud_mmr_diag, &
   liq_inconv_mmr_diag, ice_inconv_mmr_diag, &
   liq_dim_diag, ice_dim_diag, &
   liq_conv_dim_diag, ice_conv_dim_diag
  real(C_DOUBLE), intent(out), dimension(n_layer), optional :: &
   liq_frac_diag_1d, ice_frac_diag_1d, &
   liq_conv_frac_diag_1d, ice_conv_frac_diag_1d, &
   liq_incloud_mmr_diag_1d, ice_incloud_mmr_diag_1d, &
   liq_inconv_mmr_diag_1d, ice_inconv_mmr_diag_1d, &
   liq_dim_diag_1d, ice_dim_diag_1d, &
   liq_conv_dim_diag_1d, ice_conv_dim_diag_1d
  !   Liquid and ice cloud fractions, in-cloud mean mixing ratios,
  !   and effective dimension diagnostics


  integer :: l, i, k
  !   Loop variables
  logical (C_BOOL):: l_inv
  !   Local logical for field inversion
  real(C_DOUBLE) :: flux_divergence(n_profile, n_layer)
  !   Flux divergence across layer (Wm-2)

  integer :: ierr = i_normal
  character (len=errormessagelength) :: cmessage
  character (len=*), parameter :: RoutineName = 'SET_DIAG'

  Call set_diag(control, dimen, spectrum, &
  atm, cld, mcica_data, aer, bound, radout, &
  n_profile, n_layer, n_tile, &
  layer_heat_capacity, layer_heat_capacity_1d, l_invert, &
  flux_direct, flux_down, flux_up, heating_rate, &
  flux_up_tile, flux_up_blue_tile, flux_direct_blue_surf, flux_down_blue_surf, &
  flux_direct_1d, flux_down_1d, flux_up_1d, heating_rate_1d, &
  flux_up_tile_1d, flux_up_blue_tile_1d, &
  total_cloud_cover, total_cloud_fraction, total_cloud_fraction_1d, &
  liq_frac_diag, ice_frac_diag, &
  liq_conv_frac_diag, ice_conv_frac_diag, &
  liq_incloud_mmr_diag, ice_incloud_mmr_diag, &
  liq_inconv_mmr_diag, ice_inconv_mmr_diag, &
  liq_dim_diag, ice_dim_diag, &
  liq_conv_dim_diag, ice_conv_dim_diag, &
  liq_frac_diag_1d, ice_frac_diag_1d, &
  liq_conv_frac_diag_1d, ice_conv_frac_diag_1d, &
  liq_incloud_mmr_diag_1d, ice_incloud_mmr_diag_1d, &
  liq_inconv_mmr_diag_1d, ice_inconv_mmr_diag_1d, &
  liq_dim_diag_1d, ice_dim_diag_1d, &
  liq_conv_dim_diag_1d, ice_conv_dim_diag_1d)

 
 end subroutine set_diagwrap 

end module soc_set_diag