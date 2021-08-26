! Module to wrap socrates bones.


module soc_bones


USE interface_core
Use, intrinsic :: ISO_C_BINDING, Only: C_INT,C_BOOL,C_DOUBLE

implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_BONES'
contains

! The bare "bones" of radiative transfer calculations. In ancient China,
! the future weather was predicted by reading the cracks that appeared
! when oracle bones were heated. Not particularly accurate, but simple
! and cheap.


 subroutine boneswrap(n_profile, n_layer, n_tile, &
  l_cos_zen_correction, cos_zen_rts, lit_frac_rts, cos_zen_mts, lit_frac_mts, &
  l_grey_emis_correction, grey_albedo_tile, t_tile, &
  l_debug, i_profile_debug, &
  flux_direct_rts, flux_down_rts, flux_up_rts, heating_rate_rts, &
  flux_up_tile_rts, flux_up_blue_tile_rts, &
  flux_direct_surf_rts, flux_down_surf_rts, &
  flux_direct_blue_surf_rts, flux_down_blue_surf_rts, &
  flux_direct_1d_rts, flux_down_1d_rts, flux_up_1d_rts, heating_rate_1d_rts, &
  flux_up_tile_1d_rts, flux_up_blue_tile_1d_rts, &
  flux_direct_mts, flux_down_mts, flux_up_mts, heating_rate_mts, &
  flux_up_tile_mts, flux_up_blue_tile_mts, &
  flux_direct_surf_mts, flux_down_surf_mts, &
  flux_direct_blue_surf_mts, flux_down_blue_surf_mts, &
  flux_direct_1d_mts, flux_down_1d_mts, flux_up_1d_mts, heating_rate_1d_mts, &
  flux_up_tile_1d_mts, flux_up_blue_tile_1d_mts) BIND(C,name='bones')


  Use, intrinsic :: ISO_C_BINDING, only: C_INT, C_BOOL, C_DOUBLE, C_CHAR
  use realtype_rd, only: RealK
  use rad_ccf, only: stefan_boltzmann
  USE realtype_rd,  ONLY: RealK
  USE interface_core

  IMPLICIT NONE

  Integer(C_INT), intent(in) :: n_profile
  !   Number of columns to operate on
  Integer(C_INT) , intent(in) :: n_layer
  !   Number of layers for radiation
  Integer(C_INT), intent(in), optional :: n_tile
  !   Number of surface tiles

  Logical , intent(in), optional :: l_cos_zen_correction
  !   Apply simple solar zenith angle correction
  real(C_DOUBLE), intent(in), optional :: cos_zen_rts(n_profile)
  !   Mean cosine of solar zenith angle over lit fraction of radiation timestep
  real(C_DOUBLE), intent(in), optional :: lit_frac_rts(n_profile)
  !   Lit fraction of radiation timestep
  real(C_DOUBLE), intent(in), optional :: cos_zen_mts(n_profile)
  !   Mean cosine of solar zenith angle over lit fraction of model timestep
  real(C_DOUBLE), intent(in), optional :: lit_frac_mts(n_profile)
  !   Lit fraction of model timestep

  Logical , intent(in), optional :: l_grey_emis_correction
  !   Apply surface temperature correction with grey emissivity per tile
  real (C_DOUBLE), intent(in), optional :: grey_albedo_tile(:, :)
  !   Grey albedo of tiles (n_profile, n_tile)
  real (C_DOUBLE), intent(in), optional :: t_tile(:, :)
  !   Tile temperatures (n_profile, n_tile)

  Logical, intent(in), optional :: l_debug
  Integer(C_INT), intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

  ! Input radiation timestep fields:
  real(C_DOUBLE), intent(in), optional :: flux_direct_rts(n_profile, 0:n_layer)
  real(C_DOUBLE), intent(in), optional :: flux_direct_1d_rts(0:n_layer)
  !   Direct (unscattered) downwards flux (Wm-2)
  real(C_DOUBLE), intent(in), optional :: flux_down_rts(n_profile, 0:n_layer)
  real (C_DOUBLE), intent(in), optional :: flux_down_1d_rts(0:n_layer)
  !   Downwards flux (Wm-2)
  real (C_DOUBLE), intent(in), optional :: flux_up_rts(n_profile, 0:n_layer)
  real (C_DOUBLE), intent(in), optional :: flux_up_1d_rts(0:n_layer)
  !   Upwards flux (Wm-2)
  real (C_DOUBLE), intent(in), optional :: heating_rate_rts(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: heating_rate_1d_rts(n_layer)
  !   Heating rate (Ks-1)
  real (C_DOUBLE), intent(in), optional :: flux_up_tile_rts(:, :)
  real (C_DOUBLE), intent(in), optional :: flux_up_tile_1d_rts(:)
  !   Upwards flux on tiles (Wm-2) (n_profile, n_tile) and (n_tile)
  real(C_DOUBLE), intent(in), optional :: flux_up_blue_tile_rts(:, :)
  real (C_DOUBLE), intent(in), optional :: flux_up_blue_tile_1d_rts(:)
  !   Upwards blue flux on tiles (Wm-2)
  real(C_DOUBLE), intent(in), optional :: flux_direct_surf_rts(n_profile)
  !   Direct flux at the surface
  real (C_DOUBLE), intent(in), optional :: flux_down_surf_rts(n_profile)
  !   Total downward flux at the surface
  real(C_DOUBLE), intent(in), optional :: flux_direct_blue_surf_rts(n_profile)
  !   Direct blue flux at the surface
  real(C_DOUBLE), intent(in), optional :: flux_down_blue_surf_rts(n_profile)
  !   Total downward blue flux at the surface

  ! Output model timestep fields:
  real(C_DOUBLE), intent(out), optional :: flux_direct_mts(n_profile, 0:n_layer)
  real(C_DOUBLE), intent(out), optional :: flux_direct_1d_mts(0:n_layer)
  !   Direct (unscattered) downwards flux (Wm-2)
  real(C_DOUBLE), intent(out), optional :: flux_down_mts(n_profile, 0:n_layer)
  real(C_DOUBLE), intent(out), optional :: flux_down_1d_mts(0:n_layer)
  !   Downwards flux (Wm-2)
  real(C_DOUBLE), intent(out), optional :: flux_up_mts(n_profile, 0:n_layer)
  real(C_DOUBLE), intent(out), optional :: flux_up_1d_mts(0:n_layer)
  !   Upwards flux (Wm-2)
  real(C_DOUBLE), intent(out), optional :: heating_rate_mts(n_profile, n_layer)
  real(C_DOUBLE), intent(out), optional :: heating_rate_1d_mts(n_layer)
  !   Heating rate (Ks-1)
  real(C_DOUBLE), intent(out), optional :: flux_up_tile_mts(:, :)
  real(C_DOUBLE), intent(out), optional :: flux_up_tile_1d_mts(:)
  !   Upwards flux on tiles (Wm-2) (n_profile, n_tile) and (n_tile)
  real(C_DOUBLE), intent(out), optional :: flux_up_blue_tile_mts(:, :)
  real(C_DOUBLE), intent(out), optional :: flux_up_blue_tile_1d_mts(:)
  !   Upwards blue flux on tiles (Wm-2)
  real(C_DOUBLE), intent(out), optional :: flux_direct_surf_mts(n_profile)
  !   Direct flux at the surface
  real(C_DOUBLE), intent(out), optional :: flux_down_surf_mts(n_profile)
  !   Total downward flux at the surface
  real(C_DOUBLE), intent(out), optional :: flux_direct_blue_surf_mts(n_profile)
  !   Direct blue flux at the surface
  real(C_DOUBLE), intent(out), optional :: flux_down_blue_surf_mts(n_profile)
  !   Total downward blue flux at the surface

  
 Call bones(n_profile, n_layer, n_tile, &
  l_cos_zen_correction, cos_zen_rts, lit_frac_rts, cos_zen_mts, lit_frac_mts, &
  l_grey_emis_correction, grey_albedo_tile, t_tile, &
  l_debug, i_profile_debug, &
  flux_direct_rts, flux_down_rts, flux_up_rts, heating_rate_rts, &
  flux_up_tile_rts, flux_up_blue_tile_rts, &
  flux_direct_surf_rts, flux_down_surf_rts, &
  flux_direct_blue_surf_rts, flux_down_blue_surf_rts, &
  flux_direct_1d_rts, flux_down_1d_rts, flux_up_1d_rts, heating_rate_1d_rts, &
  flux_up_tile_1d_rts, flux_up_blue_tile_1d_rts, &
  flux_direct_mts, flux_down_mts, flux_up_mts, heating_rate_mts, &
  flux_up_tile_mts, flux_up_blue_tile_mts, &
  flux_direct_surf_mts, flux_down_surf_mts, &
  flux_direct_blue_surf_mts, flux_down_blue_surf_mts, &
  flux_direct_1d_mts, flux_down_1d_mts, flux_up_1d_mts, heating_rate_1d_mts, &
  flux_up_tile_1d_mts, flux_up_blue_tile_1d_mts) 


 end subroutine boneswrap

end module soc_bones
