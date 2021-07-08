PROGRAM Interface_core

  USE realtype_rd,  ONLY: RealK
  USE socrates_bones
  USE socrates_cloud_gen
  USE socrates_runes
  USE socrates_set_aer
  USE socrates_set_atm
  USE socrates_set_bound
  USE socrates_set_cld
  USE socrates_set_cld_dim
  USE socrates_set_cld_mcica
  USE socrates_set_control
  USE socrates_set_diag
  USE socrates_set_dimen
  USE socrates_set_spectrum
  USE socrates_set_diag

IMPLICIT NONE

INTERFACE

  subroutine bones_wrap(n_profile, n_layer, n_tile, &
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
  flux_up_tile_1d_mts, flux_up_blue_tile_1d_mts) BIND(C)


  Use, intrinsic :: ISO_C_BINDING
  use realtype_rd, only: RealK
  use rad_ccf, only: stefan_boltzmann

  implicit none

  Integer (C_INT), intent(in) :: n_profile
  !   Number of columns to operate on
  Integer (C_INT) , intent(in) :: n_layer
  !   Number of layers for radiation
  Integer (C_INT), intent(in), optional :: n_tile
  !   Number of surface tiles

  Logical (C_CHAR), intent(in), optional :: l_cos_zen_correction
  !   Apply simple solar zenith angle correction
  real(RealK) (C_INT), intent(in), optional :: cos_zen_rts(n_profile)
  !   Mean cosine of solar zenith angle over lit fraction of radiation timestep
  real(RealK) (C_INT), intent(in), optional :: lit_frac_rts(n_profile)
  !   Lit fraction of radiation timestep
  real(RealK) (C_INT), intent(in), optional :: cos_zen_mts(n_profile)
  !   Mean cosine of solar zenith angle over lit fraction of model timestep
  real(RealK) (C_INT), intent(in), optional :: lit_frac_mts(n_profile)
  !   Lit fraction of model timestep

  Logical (C_CHAR), intent(in), optional :: l_grey_emis_correction
  !   Apply surface temperature correction with grey emissivity per tile
  real(RealK) (C_INT), intent(in), optional :: grey_albedo_tile(:, :)
  !   Grey albedo of tiles (n_profile, n_tile)
  real(RealK) (C_INT), intent(in), optional :: t_tile(:, :)
  !   Tile temperatures (n_profile, n_tile)

  Logical (C_CHAR), intent(in), optional :: l_debug
  Integer (C_INT), intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

  ! Input radiation timestep fields:
  real(RealK) (C_INT), intent(in), optional :: flux_direct_rts(n_profile, 0:n_layer)
  real(RealK) (C_INT), intent(in), optional :: flux_direct_1d_rts(0:n_layer)
  !   Direct (unscattered) downwards flux (Wm-2)
  real(RealK) (C_INT), intent(in), optional :: flux_down_rts(n_profile, 0:n_layer)
  real(RealK) (C_INT), intent(in), optional :: flux_down_1d_rts(0:n_layer)
  !   Downwards flux (Wm-2)
  real(RealK) (C_INT), intent(in), optional :: flux_up_rts(n_profile, 0:n_layer)
  real(RealK) (C_INT), intent(in), optional :: flux_up_1d_rts(0:n_layer)
  !   Upwards flux (Wm-2)
  real(RealK) (C_INT), intent(in), optional :: heating_rate_rts(n_profile, n_layer)
  real(RealK) (C_INT), intent(in), optional :: heating_rate_1d_rts(n_layer)
  !   Heating rate (Ks-1)
  real(RealK) (C_INT), intent(in), optional :: flux_up_tile_rts(:, :)
  real(RealK) (C_INT), intent(in), optional :: flux_up_tile_1d_rts(:)
  !   Upwards flux on tiles (Wm-2) (n_profile, n_tile) and (n_tile)
  real(RealK) (C_INT), intent(in), optional :: flux_up_blue_tile_rts(:, :)
  real(RealK) (C_INT), intent(in), optional :: flux_up_blue_tile_1d_rts(:)
  !   Upwards blue flux on tiles (Wm-2)
  real(RealK) (C_INT), intent(in), optional :: flux_direct_surf_rts(n_profile)
  !   Direct flux at the surface
  real(RealK) (C_INT), intent(in), optional :: flux_down_surf_rts(n_profile)
  !   Total downward flux at the surface
  real(RealK) (C_INT), intent(in), optional :: flux_direct_blue_surf_rts(n_profile)
  !   Direct blue flux at the surface
  real(RealK) (C_INT), intent(in), optional :: flux_down_blue_surf_rts(n_profile)
  !   Total downward blue flux at the surface

  ! Output model timestep fields:
  real(RealK) (C_INT), intent(out), optional :: flux_direct_mts(n_profile, 0:n_layer)
  real(RealK) (C_INT), intent(out), optional :: flux_direct_1d_mts(0:n_layer)
  !   Direct (unscattered) downwards flux (Wm-2)
  real(RealK) (C_INT), intent(out), optional :: flux_down_mts(n_profile, 0:n_layer)
  real(RealK) (C_INT), intent(out), optional :: flux_down_1d_mts(0:n_layer)
  !   Downwards flux (Wm-2)
  real(RealK) (C_INT), intent(out), optional :: flux_up_mts(n_profile, 0:n_layer)
  real(RealK) (C_INT), intent(out), optional :: flux_up_1d_mts(0:n_layer)
  !   Upwards flux (Wm-2)
  real(RealK) (C_INT), intent(out), optional :: heating_rate_mts(n_profile, n_layer)
  real(RealK) (C_INT), intent(out), optional :: heating_rate_1d_mts(n_layer)
  !   Heating rate (Ks-1)
  real(RealK) (C_INT), intent(out), optional :: flux_up_tile_mts(:, :)
  real(RealK) (C_INT), intent(out), optional :: flux_up_tile_1d_mts(:)
  !   Upwards flux on tiles (Wm-2) (n_profile, n_tile) and (n_tile)
  real(RealK) (C_INT), intent(out), optional :: flux_up_blue_tile_mts(:, :)
  real(RealK) (C_INT), intent(out), optional :: flux_up_blue_tile_1d_mts(:)
  !   Upwards blue flux on tiles (Wm-2)
  real(RealK) (C_INT), intent(out), optional :: flux_direct_surf_mts(n_profile)
  !   Direct flux at the surface
  real(RealK) (C_INT), intent(out), optional :: flux_down_surf_mts(n_profile)
  !   Total downward flux at the surface
  real(RealK) (C_INT), intent(out), optional :: flux_direct_blue_surf_mts(n_profile)
  !   Direct blue flux at the surface
  real(RealK) (C_INT), intent(out), optional :: flux_down_blue_surf_mts(n_profile)
  !   Total downward blue flux at the surface

  CALL subroutine bones(n_profile, n_layer, n_tile, &
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
 
 end subroutine bones_wrap

 subroutine cloud_gen_wrap(nd_layer, cloud_top, n_layer, nd_profile, il1, il2, &
                     n_subcol, n1, n2, &
                     ipph, ioverlap, rand_seed, &
                     rlc_cf, rlc_cw, sigma_qcw, avg_cf, &
                     c_cloud, c_ratio, zf, xcw, &
                     n_subcol_cld, c_sub) BIND(C)

   Use, intrinsic :: ISO_C_BINDING
   use realtype_rd, only: RealK
   use yomhook, only: lhook, dr_hook
   use parkind1, only: jprb, jpim
   use ereport_mod, only: ereport

  implicit none

  ! Input
   Integer (C_INT), intent(in) :: &
    nd_layer,            & ! Size of layer dimension
    cloud_top,           & ! Top cloudy layer
    n_layer,             & ! Number of layers in GCM
    nd_profile,          & ! Dimension size for GCM columns
    il1,                 & ! Start GCM column
    il2,                 & ! End GCM column
    n_subcol,            & ! Number of sub-columns to generate
    n1, n2,              & ! Dimensions of xcw array
    ipph,                & ! Horizontal homogeneity
    ioverlap,            & ! Vertical overlap
    rand_seed(nd_profile)  ! Seed for generating random numbers

   real(RealK) (C_INT), intent(in) :: &
    zf(nd_profile, nd_layer), &
  !     Full-level (layer midpoint) pressure (Pa)
    avg_cf(nd_profile, cloud_top:nd_layer), &
  !     Cloud amount for each layer
    c_cloud(nd_profile, cloud_top:nd_layer), &
  !     Convective cloud amount for each layer
    c_ratio(nd_profile, cloud_top:nd_layer), &
  !     Convective condensate ratio
    sigma_qcw(nd_profile, cloud_top:nd_layer), &
  !     Normalized cloud condensate std. dev. (Std. dev. over mean)
    rlc_cf(nd_profile, cloud_top:nd_layer), &
  !      Cloud fraction decorrelation scale (Pa)
    rlc_cw(nd_profile, cloud_top:nd_layer), &
  !     Cloud condensate decorrelation scale (Pa)
    xcw(n1, n2)
  !     Distribution of normalised condensate amount as a function of
  !     cumulative probability (n1) and relative standard deviation (n2)

  ! Output
  Integer (C_INT), intent(out) :: n_subcol_cld(nd_profile)
  !     Number of cloudy sub-columns

  real(RealK) (C_INT), intent(inout) :: c_sub(nd_profile, cloud_top:nd_layer, n_subcol)
  !     Sub-grid cloud water content
  
 CALL subroutine cloud_gen(nd_layer, cloud_top, n_layer, nd_profile, il1, il2, &
                     n_subcol, n1, n2, &
                     ipph, ioverlap, rand_seed, &
                     rlc_cf, rlc_cw, sigma_qcw, avg_cf, &
                     c_cloud, c_ratio, zf, xcw, &
                     n_subcol_cld, c_sub)

 end subroutine cloud_gen_wrap

 subroutine runes_wrap(n_profile, n_layer, spectrum, spectrum_name, mcica_data, &
  n_cloud_layer, n_aer_mode, n_tile, &
  p_layer, t_layer, t_level, mass, density, &
  h2o, o3, &
  p_layer_1d, t_layer_1d, t_level_1d, mass_1d, density_1d, &
  h2o_1d, o3_1d, &
  co2_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
  o2_mix_ratio, so2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
  cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio, &
  t_ground, cos_zenith_angle, solar_irrad, orog_corr, &
  l_grey_albedo, grey_albedo, albedo_diff, albedo_dir, &
  l_tile, frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile, &
  cloud_frac, conv_frac, &
  liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
  liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
  liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
  liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
  liq_nc, liq_conv_nc, &
  cloud_frac_1d, conv_frac_1d, &
  liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
  liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
  liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
  liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
  liq_nc_1d, liq_conv_nc_1d, &
  cloud_vertical_decorr, conv_vertical_decorr, &
  cloud_horizontal_rsd, &
  layer_heat_capacity, layer_heat_capacity_1d, &
  i_source, i_cloud_representation, i_overlap, i_inhom, &
  i_mcica_sampling, i_st_water, i_cnv_water, i_st_ice, i_cnv_ice, i_drop_re, &
  rand_seed, &
  l_rayleigh, l_mixing_ratio, l_aerosol_mode, &
  l_invert, l_debug, i_profile_debug, &
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
  liq_conv_dim_diag_1d, ice_conv_dim_diag_1d) BIND(C)


  Use, intrinsic :: ISO_C_BINDING  
  use def_spectrum, only: StrSpecData
  use def_mcica,    only: StrMcica
  use def_control,  only: StrCtrl,  deallocate_control
  use def_dimen,    only: StrDim
  use def_atm,      only: StrAtm,   deallocate_atm
  use def_bound,    only: StrBound, deallocate_bound
  use def_cld,      only: StrCld,   deallocate_cld, deallocate_cld_prsc, &
                                  deallocate_cld_mcica
  use def_aer,      only: StrAer,   deallocate_aer, deallocate_aer_prsc
  use def_out,      only: StrOut,   deallocate_out
  use socrates_set_control,   only: set_control
  use socrates_set_dimen,     only: set_dimen
  use socrates_set_atm,       only: set_atm
  use socrates_set_bound,     only: set_bound
  use socrates_set_cld,       only: set_cld
  use socrates_set_cld_dim,   only: set_cld_dim
  use socrates_set_cld_mcica, only: set_cld_mcica
  use socrates_set_aer,       only: set_aer
  use socrates_set_diag,      only: set_diag

  use realtype_rd, only: RealK
  use ereport_mod,  only: ereport
  use errormessagelength_mod, only: errormessagelength
  use rad_pcf, only: i_normal, i_err_fatal

 implicit none

  ! Spectral data:
  type (StrSpecData), intent(in), target, optional :: spectrum
  character(len=*), intent(in), optional :: spectrum_name

  ! Mcica data
  type (StrMcica), intent(in), target, optional :: mcica_data

  Integer (C_INT), intent(in) :: n_profile
  !   Number of columns to operate on
  Integer (C_INT), intent(in) :: n_layer
  !   Number of layers for radiation
  Integer (C_INT), intent(in), optional :: n_tile
  !   Number of surface tiles
  Integer (C_INT), intent(in), optional :: n_cloud_layer
  !   Number of potentially cloudy layers
  Integer (C_INT), intent(in), optional :: n_aer_mode
  !   Number of aerosol modes

  real(RealK) (C_INT), intent(in), optional :: p_layer(n_profile, n_layer)
  real(RealK) (C_INT), intent(in), optional :: p_layer_1d(n_layer)
  !   Pressure at layer centres
  real(RealK) (C_INT), intent(in), optional :: t_layer(n_profile, n_layer)
  real(RealK) (C_INT), intent(in), optional :: t_layer_1d(n_layer)
  !   Temperature at layer centres
  real(RealK) (C_INT), intent(in), optional :: t_level(n_profile, 0:n_layer)
  real(RealK) (C_INT), intent(in), optional :: t_level_1d(0:n_layer)
  !   Temperature at layer boundaries
  real(RealK) (C_INT), intent(in), optional :: mass(n_profile, n_layer)
  real(RealK) (C_INT), intent(in), optional :: mass_1d(n_layer)
  !   Mass of layer (kg m-2)
  real(RealK) (C_INT), intent(in), optional :: density(n_profile, n_layer)
  real(RealK) (C_INT), intent(in), optional :: density_1d(n_layer)   
  !   Density of layer (kg m-3)
  real(RealK) (C_INT), intent(in), optional :: h2o(n_profile, n_layer)
  real(RealK) (C_INT), intent(in), optional :: h2o_1d(n_layer)
  !   Mass mixing ratio of water vapour
  real(RealK) (C_INT), intent(in), optional :: o3(n_profile, n_layer)
  real(RealK) (C_INT), intent(in), optional :: o3_1d(n_layer)
  !   Mass mixing ratio of ozone

  real(RealK) (C_INT), intent(in), optional :: &
   co2_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
   o2_mix_ratio, so2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
   cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio
  !   Trace gas mass mixing ratios

  real(RealK) (C_INT), intent(in), optional :: t_ground(n_profile)
  !   Effective radiative temperature over whole grid-box
  real(RealK) (C_INT), intent(in), optional :: cos_zenith_angle(n_profile)
  !   Cosine of solar zenith angle
  real(RealK) (C_INT), intent(in), optional :: solar_irrad(n_profile)
  !   Solar irradiance at top-of-atmosphere (mean over timestep)
  real(RealK) (C_INT), intent(in), optional :: orog_corr(n_profile)
  !   Orographic correction factor

  Logical (C_CHAR), intent(in), optional :: l_grey_albedo
  !   Set a single grey albedo / emissivity for the surface
  real(RealK) (C_INT), intent(in), optional :: grey_albedo
  !   Grey surface albedo

  real(RealK) (C_INT) , intent(in), optional :: albedo_diff(:, :)
  !   Spectral diffuse albedo (n_profile, n_band)
  real(RealK) (C_INT), intent(in), optional :: albedo_dir(:, :)
  !   Spectral direct albedo (n_profile, n_band)

  Logical (C_CHAR), intent(in), optional :: l_tile
  !   Use tiled surface properties
  real(RealK) (C_INT), intent(in), optional :: frac_tile(:, :)
  !   Tile fractions (n_profile, n_tile)
  real(RealK) (C_INT), intent(in), optional :: albedo_diff_tile(:, :, :)
  !   Diffuse tile albedo (n_profile, n_tile, n_band)
  real(RealK) (C_INT), intent(in), optional :: albedo_dir_tile(:, :, :)
  !   Direct tile albedo (n_profile, n_tile, n_band)
  real(RealK) (C_INT), intent(in), optional :: t_tile(:, :)
  !   Tile temperatures (n_profile, n_tile)

  real(RealK) (C_INT), intent(in), dimension (n_profile, n_layer), optional :: &
   cloud_frac, conv_frac, &
   liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
   liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
   liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
   liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
   liq_nc, liq_conv_nc
  real(RealK) (C_INT), intent(in), dimension (n_layer), optional :: &
   cloud_frac_1d, conv_frac_1d, &
   liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
   liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
   liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
   liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
   liq_nc_1d, liq_conv_nc_1d
  !   Liquid and ice cloud fractions, gridbox mean mixing ratios,
  !   effective dimensions, relative standard deviation of condensate,
  !   and number concentration

  real(RealK) (C_INT), intent(in), optional :: cloud_vertical_decorr
  !   Decorrelation pressure scale for cloud vertical overlap
  real(RealK) (C_INT), intent(in), optional :: conv_vertical_decorr
  !   Decorrelation pressure scale for convective cloud vertical overlap
  real(RealK) (C_INT), intent(in), optional :: cloud_horizontal_rsd
  !   Relative standard deviation of sub-grid cloud condensate

  real(RealK) (C_DOUBLE), intent(in), optional :: layer_heat_capacity(n_profile, n_layer)
  real(RealK) (C_DOUBLE), intent(in), optional :: layer_heat_capacity_1d(n_layer)
  !   Heat capacity of layer

  Integer (C_INT), intent(in), optional :: i_source
  !   Select source of radiation
  Integer (C_INT), intent(in), optional :: &
  i_cloud_representation, i_overlap, i_inhom, &
  i_mcica_sampling, i_st_water, i_st_ice, i_cnv_water, i_cnv_ice, i_drop_re
  !   Select treatment of cloud
  Integer (C_INT), intent(in), optional :: rand_seed(n_profile)
  !   Random seed for cloud generator

  Logical (C_CHAR), intent(in), optional :: l_rayleigh
  !   Include Rayleigh scattering
  Logical (C_CHAR), intent(in), optional :: l_mixing_ratio
  !   Assume mass mixing ratios are with respect to dry mass
  Logical (C_CHAR), intent(in), optional :: l_aerosol_mode
  !   Include aerosol optical properties specified by mode

  Logical (C_CHAR), intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical

  Logical (C_CHAR), intent(in), optional :: l_debug
  integer, intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information


  ! Output fields:
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_direct(n_profile, 0:n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_direct_1d(0:n_layer)
  !   Direct (unscattered) downwards flux (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_down(n_profile, 0:n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_down_1d(0:n_layer)
  !   Downwards flux (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up(n_profile, 0:n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_1d(0:n_layer)
  !   Upwards flux (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: heating_rate(n_profile, n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: heating_rate_1d(n_layer)
  !   Heating rate (Ks-1)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_tile(:, :) ! (n_profile, n_tile)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_tile_1d(:) ! (n_tile)
  !   Upwards flux on tiles (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_blue_tile(:, :)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_blue_tile_1d(:)
  !   Upwards blue flux on tiles (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_direct_blue_surf(n_profile)
  !   Direct blue flux at the surface
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_down_blue_surf(n_profile)
  !   Total downward blue flux at the surface
  real(RealK) (C_DOUBLE), intent(out), optional :: total_cloud_cover(n_profile)
  !   Total cloud cover
  real(RealK) (C_DOUBLE), intent(out), optional :: total_cloud_fraction(n_profile, n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: total_cloud_fraction_1d(n_layer)
  !   Total cloud fraction in layers
  real(RealK) (C_DOUBLE), intent(out), dimension(n_profile, n_layer), optional :: &
   liq_frac_diag, ice_frac_diag, &
   liq_conv_frac_diag, ice_conv_frac_diag, &
   liq_incloud_mmr_diag, ice_incloud_mmr_diag, &
   liq_inconv_mmr_diag, ice_inconv_mmr_diag, &
   liq_dim_diag, ice_dim_diag, &
   liq_conv_dim_diag, ice_conv_dim_diag
  real(RealK), intent(out), dimension(n_layer), optional :: &
   liq_frac_diag_1d, ice_frac_diag_1d, &
   liq_conv_frac_diag_1d, ice_conv_frac_diag_1d, &
   liq_incloud_mmr_diag_1d, ice_incloud_mmr_diag_1d, &
   liq_inconv_mmr_diag_1d, ice_inconv_mmr_diag_1d, &
   liq_dim_diag_1d, ice_dim_diag_1d, &
   liq_conv_dim_diag_1d, ice_conv_dim_diag_1d
  !   Liquid and ice cloud fractions, in-cloud mean mixing ratios,
  !   and effective dimension diagnostics

  ! Spectral data:
  type(StrSpecData), pointer :: spec => null()

  ! Mcica data:
  type(StrMcica), pointer :: mcica => null()
  type(StrMcica), target :: mcica_dummy

  ! Controlling options:
  type(StrCtrl) :: control

  ! Dimensions:
  type(StrDim) :: dimen

  ! Atmospheric properties:
  type(StrAtm) :: atm

  ! Boundary conditions:
  type(StrBound) :: bound

  ! Cloud properties:
  type(StrCld) :: cld

  ! Aerosol properties:
  type(StrAer) :: aer

  ! Output fields from core radiation code:
  type(StrOut) :: radout

  Integer (C_INT) :: id_spec, id_mcica
  !   Loop variables
  logical (C_INT) :: l_blue_flux_surf
  !   Output blue fluxes if requested

  Integer (C_INT) :: ierr = i_normal
  character ( C_CHAR) (len=errormessagelength) :: cmessage
  character (C_CHAR)(len=*), parameter :: RoutineName = 'RUNES'

  CALL subroutine runes(n_profile, n_layer, spectrum, spectrum_name, mcica_data, &
  n_cloud_layer, n_aer_mode, n_tile, &
  p_layer, t_layer, t_level, mass, density, &
  h2o, o3, &
  p_layer_1d, t_layer_1d, t_level_1d, mass_1d, density_1d, &
  h2o_1d, o3_1d, &
  co2_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
  o2_mix_ratio, so2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
  cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio, &
  t_ground, cos_zenith_angle, solar_irrad, orog_corr, &
  l_grey_albedo, grey_albedo, albedo_diff, albedo_dir, &
  l_tile, frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile, &
  cloud_frac, conv_frac, &
  liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
  liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
  liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
  liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
  liq_nc, liq_conv_nc, &
  cloud_frac_1d, conv_frac_1d, &
  liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
  liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
  liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
  liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
  liq_nc_1d, liq_conv_nc_1d, &
  cloud_vertical_decorr, conv_vertical_decorr, &
  cloud_horizontal_rsd, &
  layer_heat_capacity, layer_heat_capacity_1d, &
  i_source, i_cloud_representation, i_overlap, i_inhom, &
  i_mcica_sampling, i_st_water, i_cnv_water, i_st_ice, i_cnv_ice, i_drop_re, &
  rand_seed, &
  l_rayleigh, l_mixing_ratio, l_aerosol_mode, &
  l_invert, l_debug, i_profile_debug, &
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


end subroutine runes_wrap
 
 subroutine set_aer_wrap(aer, control, dimen, spectrum, &
  n_profile, n_layer, n_aer_mode, &
  aer_mix_ratio, aer_absorption, aer_scattering, aer_asymmetry) BIND(C)

  Use, intrinsic :: ISO_C_BINDING 
  use def_aer,      only: StrAer, allocate_aer, allocate_aer_prsc
  use def_control,  only: StrCtrl
  use def_dimen,    only: StrDim
  use def_spectrum, only: StrSpecData
  use realtype_rd,  only: RealK

  implicit none


  ! Aerosol properties:
  type(StrAer),      intent(out) :: aer

  ! Control options:
  type(StrCtrl),     intent(in)  :: control

  ! Dimensions:
  type(StrDim),      intent(in)  :: dimen

  ! Spectral data:
  type(StrSpecData), intent(in)  :: spectrum

  Integer (C_INT), intent(in) :: n_profile
  Integer (C_INT), intent(in) :: n_layer

  Integer (C_INT), intent(in), optional :: n_aer_mode
  !   Number of aerosol modes

  real(RealK) (C_DOUBLE), intent(in), optional :: aer_mix_ratio(:, :, :)
  !   Mass-mixing ratio (n_profile, n_layer, n_mode)
  real(RealK) (C_DOUBLE), intent(in), optional :: aer_absorption(:, :, :, :)
  !   Aerosol absorption (n_profile, n_layer, n_mode, n_band)
  real(RealK) (C_DOUBLE), intent(in), optional :: aer_scattering(:, :, :, :)
  !   Aerosol scattering (n_profile, n_layer, n_mode, n_band)
  real(RealK) (C_DOUBLE), intent(in), optional :: aer_asymmetry(:, :, :, :)
  !   Aerosol asymmetry (n_profile, n_layer, n_mode, n_band)

  CALL subroutine set_aer(aer, control, dimen, spectrum, &
  n_profile, n_layer, n_aer_mode, &
  aer_mix_ratio, aer_absorption, aer_scattering, aer_asymmetry)


 end subroutine set_aer_wrap

 subroutine set_atm_wrap(atm, dimen, spectrum, n_profile, n_layer, &
  p_layer, t_layer, mass, density, p_level, t_level, r_layer, r_level, &
  p_layer_1d, t_layer_1d, mass_1d, density_1d, p_level_1d, t_level_1d, &
  r_layer_1d, r_level_1d, &
  h2o, co2, o3, n2o, ch4, o2, so2, n2, cfc11, cfc12, cfc113, hcfc22, hfc134a, &
  h2o_1d, co2_1d, o3_1d, n2o_1d, ch4_1d, o2_1d, so2_1d, n2_1d, cfc11_1d, &
  cfc12_1d, cfc113_1d, hcfc22_1d, hfc134a_1d, &
  h2o_mix_ratio, co2_mix_ratio, o3_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
  o2_mix_ratio, so2_mix_ratio, n2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
  cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio, &
  l_h2o_well_mixed, l_co2_well_mixed, l_o3_well_mixed, l_n2o_well_mixed, &
  l_ch4_well_mixed, l_o2_well_mixed, l_so2_well_mixed, l_n2_well_mixed, &
  l_cfc11_well_mixed, l_cfc12_well_mixed, l_cfc113_well_mixed, &
  l_hcfc22_well_mixed, l_hfc134a_well_mixed, &
  l_invert, l_debug, i_profile_debug) BIND(C)

 Use, intrinsic :: ISO_C_BINDING 
 use def_atm,      only: StrAtm, allocate_atm
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
 use realtype_rd,  only: RealK
 use gas_list_pcf, only: ip_h2o, ip_co2, ip_o3, ip_n2o, ip_ch4, ip_o2, ip_so2, &
  ip_n2, ip_cfc11, ip_cfc12, ip_cfc113, ip_hcfc22, ip_hfc134a

 implicit none


  ! Atmospheric properties:
  type(StrAtm),      intent(out) :: atm

  ! Dimensions:
  type(StrDim),      intent(in)  :: dimen

  ! Spectral data:
  type(StrSpecData), intent(in)  :: spectrum

  Integer (C_INT), intent(in) :: n_profile
  !   Number of atmospheric profiles for radiation calculations
  Integer (C_INT), intent(in) :: n_layer
  !   Number of atmospheric layers for radiation calculations

  real(RealK) (C_DOUBLE), intent(in), optional :: p_layer(:, :), p_layer_1d(:)
  !   Pressure at layer centres
  real(RealK) (C_DOUBLE), intent(in), optional :: t_layer(:, :), t_layer_1d(:)
  !   Temperature at layer centres
  real(RealK) (C_DOUBLE), intent(in), optional :: mass(:, :), mass_1d(:)
  !   Mass of layer (kg m-2)
  real(RealK) (C_DOUBLE), intent(in), optional :: density(:, :), density_1d(:)
  !   Density of layer (kg m-3)
  real(RealK) (C_DOUBLE), intent(in), optional :: p_level(:, 0:), p_level_1d(0:)
  !   Pressure at layer boundaries
  real(RealK) (C_DOUBLE), intent(in), optional :: t_level(:, 0:), t_level_1d(0:)
  !   Temperature at layer boundaries
  real(RealK) (C_DOUBLE), intent(in), optional :: r_layer(:, :), r_layer_1d(:)
  !   Radius (height from centre of planet) at layer centres
  real(RealK) (C_DOUBLE), intent(in), optional :: r_level(:, 0:), r_level_1d(0:)
  !   Radius (height from centre of planet) at layer boundaries

  real(RealK) (C_DOUBLE), intent(in), dimension(:, :), optional :: &
   h2o, co2, o3, n2o, ch4, o2, so2, n2, cfc11, cfc12, cfc113, hcfc22, hfc134a
  !   Full field mass mixing ratios

  real(RealK) (C_DOUBLE), intent(in), dimension(:), optional :: &
   h2o_1d, co2_1d, o3_1d, n2o_1d, ch4_1d, o2_1d, so2_1d, n2_1d, cfc11_1d, &
   cfc12_1d, cfc113_1d, hcfc22_1d, hfc134a_1d
   !   1d mass mixing ratios

  real(RealK) (C_DOUBLE), intent(in), optional :: &
   h2o_mix_ratio, co2_mix_ratio, o3_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
   o2_mix_ratio, so2_mix_ratio, n2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
   cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio
  !   Well mixed mass mixing ratios

  Logical (C_CHAR), intent(in), optional :: &
   l_h2o_well_mixed, l_co2_well_mixed, l_o3_well_mixed, l_n2o_well_mixed, &
   l_ch4_well_mixed, l_o2_well_mixed, l_so2_well_mixed, l_n2_well_mixed, &
   l_cfc11_well_mixed, l_cfc12_well_mixed, l_cfc113_well_mixed, &
   l_hcfc22_well_mixed, l_hfc134a_well_mixed 
  !   Flag to use the well mixed ratios

  Logical (C_CHAR), intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical

  Logical (C_CHAR), intent(in), optional :: l_debug
  integer, intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

  CALL subroutine set_atm(atm, dimen, spectrum, n_profile, n_layer, &
  p_layer, t_layer, mass, density, p_level, t_level, r_layer, r_level, &
  p_layer_1d, t_layer_1d, mass_1d, density_1d, p_level_1d, t_level_1d, &
  r_layer_1d, r_level_1d, &
  h2o, co2, o3, n2o, ch4, o2, so2, n2, cfc11, cfc12, cfc113, hcfc22, hfc134a, &
  h2o_1d, co2_1d, o3_1d, n2o_1d, ch4_1d, o2_1d, so2_1d, n2_1d, cfc11_1d, &
  cfc12_1d, cfc113_1d, hcfc22_1d, hfc134a_1d, &
  h2o_mix_ratio, co2_mix_ratio, o3_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
  o2_mix_ratio, so2_mix_ratio, n2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
  cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio, &
  l_h2o_well_mixed, l_co2_well_mixed, l_o3_well_mixed, l_n2o_well_mixed, &
  l_ch4_well_mixed, l_o2_well_mixed, l_so2_well_mixed, l_n2_well_mixed, &
  l_cfc11_well_mixed, l_cfc12_well_mixed, l_cfc113_well_mixed, &
  l_hcfc22_well_mixed, l_hfc134a_well_mixed, &
  l_invert, l_debug, i_profile_debug)

 
 end subroutine set_atm_wrap

 subroutine set_bound_wrap(bound, control, dimen, spectrum, &
  n_profile, n_tile, &
  t_ground, cos_zenith_angle, solar_irrad, orog_corr, &
  l_grey_albedo, grey_albedo, albedo_diff, albedo_dir, &
  frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile, &
  l_debug, i_profile_debug) BIND(C)

 Use, intrinsic :: ISO_C_BINDING 
 use def_bound,    only: StrBound, allocate_bound
 use def_control,  only: StrCtrl
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
 use realtype_rd,  only: RealK
 use rad_pcf,      only: &
  ip_solar, ip_infra_red, ip_surf_alb_diff, ip_surf_alb_dir

 implicit none


  ! Boundary properties:
  type(StrBound),    intent(out) :: bound

  ! Control options:
  type(StrCtrl),     intent(in)  :: control

  ! Dimensions:
  type(StrDim),      intent(in)  :: dimen

  ! Spectral data:
  type(StrSpecData), intent(in)  :: spectrum

  integer (C_INT), intent(in) :: n_profile
  !   Number of atmospheric profiles for radiation calculations
  integer (C_INT), intent(in), optional :: n_tile
  !   Number of surface tiles for radiation calculations

  real(RealK) (C_DOUBLE), intent(in), optional :: t_ground(n_profile)
  !   Effective radiative temperature over whole grid-box
  real(RealK) (C_DOUBLE), intent(in), optional :: cos_zenith_angle(n_profile)
  !   Cosine of solar zenith angle
  real(RealK) (C_DOUBLE), intent(in), optional :: solar_irrad(n_profile)
  !   Solar irradiance at top-of-atmosphere (mean over timestep)
  real(RealK) (C_DOUBLE), intent(in), optional :: orog_corr(n_profile)
  !   Orographic correction factor

  Logical (C_CHAR), intent(in), optional :: l_grey_albedo
  !   Set a single grey albedo for the surface
  real(RealK) (C_DOUBLE), intent(in), optional :: grey_albedo
  !   Grey surface albedo

  real(RealK) (C_DOUBLE), intent(in), optional :: &
   albedo_diff(n_profile, spectrum%dim%nd_band), &
   albedo_dir(n_profile, spectrum%dim%nd_band)

  real(RealK) (C_DOUBLE)  , intent(in), optional :: &
   frac_tile(n_profile, dimen%nd_tile), &
   albedo_diff_tile(n_profile, dimen%nd_tile, spectrum%dim%nd_band), &
   albedo_dir_tile(n_profile, dimen%nd_tile, spectrum%dim%nd_band), &
   t_tile(n_profile, dimen%nd_tile)

  Logical (C_CHAR), intent(in), optional :: l_debug
  integer (C_INT), intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

  CALL subroutine set_bound(bound, control, dimen, spectrum, &
  n_profile, n_tile, &
  t_ground, cos_zenith_angle, solar_irrad, orog_corr, &
  l_grey_albedo, grey_albedo, albedo_diff, albedo_dir, &
  frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile, &
  l_debug, i_profile_debug)

 end subroutine set_bound_wrap

 subroutine set_cld_wrap(cld, control, dimen, spectrum, atm, &
  cloud_frac, conv_frac, &
  liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
  liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
  liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
  cloud_frac_1d, conv_frac_1d, &
  liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
  liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
  liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
  cloud_vertical_decorr, conv_vertical_decorr, cloud_horizontal_rsd, &
  l_invert, l_debug, i_profile_debug) BIND(C)


 Use, intrinsic :: ISO_C_BINDING 
 use def_cld,      only: StrCld, allocate_cld, allocate_cld_prsc
 use def_control,  only: StrCtrl
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
 use def_atm,      only: StrAtm 
 use realtype_rd,  only: RealK
 use rad_pcf,      only: &
  ip_cloud_homogen, ip_cloud_ice_water, ip_cloud_conv_strat, ip_cloud_csiw, &
  ip_cloud_combine_homogen, ip_cloud_combine_ice_water, &
  ip_cloud_split_homogen, ip_cloud_split_ice_water, &
  ip_clcmp_st_water, ip_clcmp_st_ice, ip_clcmp_cnv_water, ip_clcmp_cnv_ice, &
  ip_phase_water, ip_phase_ice, ip_cloud_type_homogen, &
  ip_cloud_type_water, ip_cloud_type_ice, &
  ip_cloud_type_strat, ip_cloud_type_conv, &
  ip_cloud_type_sw, ip_cloud_type_si, ip_cloud_type_cw, ip_cloud_type_ci, &
  ip_drop_unparametrized, ip_ice_unparametrized, &
  ip_scaling, ip_cairns, ip_mcica, ip_tripleclouds_2019, &
  i_normal, i_err_fatal
 use ereport_mod,  only: ereport
 use errormessagelength_mod, only: errormessagelength

 implicit none


  ! Cloud properties:
  type(StrCld),      intent(out) :: cld

  ! Control options:
  type(StrCtrl),     intent(in)  :: control

  ! Dimensions:
  type(StrDim),      intent(in)  :: dimen

  ! Spectral data:
  type(StrSpecData), intent(in)  :: spectrum

  ! Atmospheric properties:
  type(StrAtm),      intent(in)  :: atm

  real(RealK) (C_DOUBLE), intent(in), dimension(:, :), optional :: &
   cloud_frac, conv_frac, &
   liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
   liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
   liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd
  !   Liquid and ice cloud fractions, gridbox mean mixing ratios,
  !   and relative standard deviation of condensate

  real(RealK) (C_DOUBLE), intent(in), dimension(:), optional :: &
   cloud_frac_1d, conv_frac_1d, &
   liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
   liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
   liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d
  !   Liquid and ice cloud fractions, gridbox mean mixing ratios,
  !   and relative standard deviation of condensate input as 1d fields

  real(RealK) (C_DOUBLE), intent(in), optional :: cloud_vertical_decorr
  !   Decorrelation pressure scale for cloud vertical overlap
  real(RealK) (C_DOUBLE), intent(in), optional :: conv_vertical_decorr
  !   Decorrelation pressure scale for convective cloud vertical overlap
  real(RealK) (C_DOUBLE), intent(in), optional :: cloud_horizontal_rsd
  !   Relative standard deviation of sub-grid cloud condensate

  Logical (C_CHAR), intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical

  Logical (C_CHAR), intent(in), optional :: l_debug
  integer, intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

  CALL subroutine set_cld(cld, control, dimen, spectrum, atm, &
  cloud_frac, conv_frac, &
  liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
  liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
  liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
  cloud_frac_1d, conv_frac_1d, &
  liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
  liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
  liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
  cloud_vertical_decorr, conv_vertical_decorr, cloud_horizontal_rsd, &
  l_invert, l_debug, i_profile_debug)


 end subroutine set_cld_wrap 

 subroutine set_cld_dim_wrap(cld, control, dimen, spectrum, atm, &
  liq_nc, liq_conv_nc, &
  liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
  liq_nc_1d, liq_conv_nc_1d, &
  liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
  l_invert, l_debug, i_profile_debug) BIND(C)

 Use, intrinsic :: ISO_C_BINDING 
 use def_cld,      only: StrCld
 use def_control,  only: StrCtrl
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
 use def_atm,      only: StrAtm
 use realtype_rd,  only: RealK
 use rad_pcf,      only: &
  ip_cloud_split_homogen, ip_cloud_split_ice_water, &
  ip_clcmp_st_water, ip_clcmp_st_ice, ip_clcmp_cnv_water, ip_clcmp_cnv_ice, &
  ip_re_external, &
  i_normal, i_err_fatal
 use ereport_mod,  only: ereport
 use errormessagelength_mod, only: errormessagelength

 implicit none


  ! Cloud properties:
  type(StrCld),      intent(inout) :: cld

  ! Control options:
  type(StrCtrl),     intent(in)  :: control

  ! Dimensions:
  type(StrDim),      intent(in)  :: dimen

  ! Spectral data:
  type(StrSpecData), intent(in)  :: spectrum

  ! Atmospheric properties:
  type(StrAtm),      intent(in)  :: atm

  real(RealK) (C_DOUBLE), intent(in), dimension(:, :), optional :: &
   liq_nc, liq_conv_nc, &
   liq_dim, ice_dim, liq_conv_dim, ice_conv_dim
  real(RealK) (C_DOUBLE), intent(in), dimension(:), optional :: &
   liq_nc_1d, liq_conv_nc_1d, &
   liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d
  !   Liquid number concentration, liquid and ice effective dimensions

  Logical (C_CHAR), intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical

  logical(C_CHAR), intent(in), optional :: l_debug
  integer, intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

  CALL subroutine set_cld_dim(cld, control, dimen, spectrum, atm, &
  liq_nc, liq_conv_nc, &
  liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
  liq_nc_1d, liq_conv_nc_1d, &
  liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
  l_invert, l_debug, i_profile_debug)

 end subroutine set_cld_dim_wrap
 
 subroutine set_cld_mcica_wrap(cld, mcica_data, control, dimen, spectrum, atm, &
  rand_seed) BIND(C)
 
  Use, intrinsic :: ISO_C_BINDING 
  use def_cld,      only: StrCld, allocate_cld_mcica
  use def_mcica,    only: StrMcica, ip_mcica_full_sampling, &
  ip_mcica_single_sampling, ip_mcica_optimal_sampling
  use def_control,  only: StrCtrl
  use def_dimen,    only: StrDim
  use def_spectrum, only: StrSpecData
  use def_atm,      only: StrAtm
  use realtype_rd,  only: RealK
  use rad_pcf,      only: &
  ip_cloud_off, ip_cloud_mcica, ip_solar, &
  i_normal, i_err_fatal
 use ereport_mod,  only: ereport
 use errormessagelength_mod, only: errormessagelength

 use socrates_cloud_gen, only: cloud_gen

 implicit none


  ! Cloud properties:
  type(StrCld),      intent(inout) :: cld

  ! Mcica data:
  type(StrMcica),    intent(in) :: mcica_data

  ! Control options:
  type(StrCtrl),     intent(in) :: control

  ! Dimensions:
  type(StrDim),      intent(in) :: dimen

  ! Spectral data:
  type(StrSpecData), intent(in) :: spectrum

  ! Atmospheric properties:
  type(StrAtm),      intent(in) :: atm

  integer (C_INT), intent(in), optional :: rand_seed(:)
  !   Random seed for cloud generator

  real(RealK) (C_DOUBLE), dimension(dimen%nd_profile, dimen%id_cloud_top:dimen%nd_layer) :: &
  dp_corr_cloud, &
  !   Cloud fraction decorrelation length
  dp_corr_cond, &
  !   Cloud condensate decorrelation length
  cond_rsd, &
  !   Relative standard deviation of sub-grid cloud condensate
  sum_weight, weight
  !   Working arrays for calculating the weighted cond_rsd
  integer (C_INT) :: rnd_seed(dimen%nd_profile)
  !   Random seed

  integer :: l, i, j, k
  integer :: i_k, i_band, n_k
  integer :: n_subcol_fill

  integer                      :: ierr = i_normal
  character (len=*), parameter :: RoutineName = 'SET_CLD_MCICA'
  character (len=errormessagelength) :: cmessage

 CALL subroutine set_cld_mcica(cld, mcica_data, control, dimen, spectrum, atm, &
  rand_seed)

 end subroutine set_cld_mcica_wrap

 subroutine set_control_wrap(control, spectrum, l_set_defaults, &
  l_rayleigh, l_gas, l_continuum, l_cont_gen, l_orog, l_solvar, &
  l_rescale, l_ir_source_quad, l_mixing_ratio, &
  l_aerosol, l_aerosol_mode, l_aerosol_ccn, &
  l_tile, l_clear, &
  l_flux_up_band, l_flux_down_band, &
  l_flux_up_clear_band, l_flux_down_clear_band, &
  l_blue_flux_surf, &
  n_tile, n_cloud_layer, n_aer_mode, &
  isolir, i_cloud_representation, i_overlap, i_inhom, i_mcica_sampling, &
  i_st_water, i_cnv_water, i_st_ice, i_cnv_ice, i_drop_re ) BIND(C)

 Use, intrinsic :: ISO_C_BINDING 
 use def_control,  only: StrCtrl, allocate_control
 use def_spectrum, only: StrSpecData
 use realtype_rd,  only: RealK
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
  type(StrCtrl), intent(inout) :: control

  ! Spectral data:
  type(StrSpecData), intent(in), optional :: spectrum

  Logical (C_CHAR), intent(in), optional :: l_set_defaults, &
   l_rayleigh, l_gas, l_continuum, l_cont_gen, l_orog, l_solvar, &
   l_rescale, l_ir_source_quad, l_mixing_ratio, &
   l_aerosol, l_aerosol_mode, l_aerosol_ccn, &
   l_tile, l_clear, &
   l_flux_up_band, l_flux_down_band, &
   l_flux_up_clear_band, l_flux_down_clear_band, &
   l_blue_flux_surf

  integer (C_INT), intent(in), optional :: n_tile, n_cloud_layer, n_aer_mode

  integer (C_INT), intent(in), optional :: isolir, &
   i_cloud_representation, i_overlap, i_inhom, i_mcica_sampling, &
   i_st_water, i_cnv_water, i_st_ice, i_cnv_ice, i_drop_re

  CALL subroutine set_control(control, spectrum, l_set_defaults, &
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

 end subroutine set_control_wrap

 subroutine set_diag_wrap(control, dimen, spectrum, &
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
  liq_conv_dim_diag_1d, ice_conv_dim_diag_1d) BIND(C)

 Use, intrinsic :: ISO_C_BINDING 
 use def_control,  only: StrCtrl
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
 use def_atm,      only: StrAtm
 use def_cld,      only: StrCld
 use def_mcica,    only: StrMcica
 use def_aer,      only: StrAer
 use def_bound,    only: StrBound
 use def_out,      only: StrOut

 use realtype_rd, only: RealK
 use ereport_mod,  only: ereport
 use errormessagelength_mod, only: errormessagelength
 use rad_pcf, only: i_normal, i_err_fatal, ip_cloud_off, ip_mcica, &
                   ip_clcmp_st_water, ip_clcmp_st_ice, &
                   ip_clcmp_cnv_water, ip_clcmp_cnv_ice

 implicit none

  ! Control options:
  type(StrCtrl),      intent(in) :: control

  ! Dimensions:
  type(StrDim),       intent(in) :: dimen

  ! Spectral data:
  type (StrSpecData), intent(in) :: spectrum

  ! Atmospheric properties:
  type(StrAtm),       intent(in) :: atm

  ! Cloud properties:
  type(StrCld),       intent(in) :: cld

  ! MCICA data:
  type(StrMcica),    intent(in) :: mcica_data

  ! Aerosol properties:
  type(StrAer),       intent(in) :: aer

  ! Boundary conditions:
  type(StrBound),     intent(in) :: bound

  ! Output fields from core radiation code:
  type(StrOut),       intent(in) :: radout

  integer (C_INT), intent(in) :: n_profile
  !   Number of columns to operate on
  integer (C_INT), intent(in) :: n_layer
  !   Number of layers for radiation
  integer (C_INT), intent(in), optional :: n_tile
  !   Number of surface tiles

  real(RealK) (C_DOUBLE), intent(in), optional :: layer_heat_capacity(n_profile, n_layer)
  real(RealK) (C_DOUBLE), intent(in), optional :: layer_heat_capacity_1d(n_layer)
  !   Heat capacity of layer

  Logical (C_CHAR), intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical


  ! Output fields:
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_direct(n_profile, 0:n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_direct_1d(0:n_layer)
  !   Direct (unscattered) downwards flux (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_down(n_profile, 0:n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_down_1d(0:n_layer)
  !   Downwards flux (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up(n_profile, 0:n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_1d(0:n_layer)
  !   Upwards flux (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: heating_rate(n_profile, n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: heating_rate_1d(n_layer)
  !   Heating rate (Ks-1)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_tile(:, :) ! (n_profile, n_tile)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_tile_1d(:) ! (n_tile)
  !   Upwards flux on tiles (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_blue_tile(:, :)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_up_blue_tile_1d(:)
  !   Upwards blue flux on tiles (Wm-2)
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_direct_blue_surf(n_profile)
  !   Direct blue flux at the surface
  real(RealK) (C_DOUBLE), intent(out), optional :: flux_down_blue_surf(n_profile)
  !   Total downward blue flux at the surface
  real(RealK) (C_DOUBLE), intent(out), optional :: total_cloud_cover(n_profile)
  !   Total cloud cover
  real(RealK) (C_DOUBLE), intent(out), optional :: total_cloud_fraction(n_profile, n_layer)
  real(RealK) (C_DOUBLE), intent(out), optional :: total_cloud_fraction_1d(n_layer)
  !   Total cloud fraction in layers
  real(RealK) (C_DOUBLE), intent(out), dimension(n_profile, n_layer), optional :: &
   liq_frac_diag, ice_frac_diag, &
   liq_conv_frac_diag, ice_conv_frac_diag, &
   liq_incloud_mmr_diag, ice_incloud_mmr_diag, &
   liq_inconv_mmr_diag, ice_inconv_mmr_diag, &
   liq_dim_diag, ice_dim_diag, &
   liq_conv_dim_diag, ice_conv_dim_diag
  real(RealK) (C_DOUBLE), intent(out), dimension(n_layer), optional :: &
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
  logical  (C_CHAR):: l_inv
  !   Local logical for field inversion
  real(RealK) (C_DOUBLE) :: flux_divergence(n_profile, n_layer)
  !   Flux divergence across layer (Wm-2)

  integer :: ierr = i_normal
  character (len=errormessagelength) :: cmessage
  character (len=*), parameter :: RoutineName = 'SET_DIAG'

  CALL subroutine set_diag(control, dimen, spectrum, &
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

 end subroutine set_diag_wrap 

 subroutine set_dimen_wrap(dimen, control, n_profile, n_layer, mcica_data, &
  n_channel, n_tile, n_cloud_layer, n_aer_mode, &
  n_direction, n_viewing_level, n_brdf_basis_fnc, n_brdf_trunc, &
  n_profile_aerosol_prsc, n_profile_cloud_prsc, &
  n_opt_level_aerosol_prsc, n_opt_level_cloud_prsc) BIND(C)

 Use, intrinsic :: ISO_C_BINDING 
 use def_dimen,   only: StrDim
 use def_control, only: StrCtrl
 use def_mcica,   only: StrMcica, ip_mcica_full_sampling, &
  ip_mcica_single_sampling, ip_mcica_optimal_sampling
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
  type(StrCtrl), intent(in) :: control

  ! Mcica data:
  type(StrMcica), intent(in), optional :: mcica_data

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

  CALL subroutine set_dimen(dimen, control, n_profile, n_layer, mcica_data, &
  n_channel, n_tile, n_cloud_layer, n_aer_mode, &
  n_direction, n_viewing_level, n_brdf_basis_fnc, n_brdf_trunc, &
  n_profile_aerosol_prsc, n_profile_cloud_prsc, &
  n_opt_level_aerosol_prsc, n_opt_level_cloud_prsc)

 end subroutine set_dimen_wrap

 subroutine set_spectrum_wrap(n_instances, spectrum, spectrum_name, spectral_file, &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases, &
  wavelength_blue) BIND(C)

 Use, intrinsic :: ISO_C_BINDING 
 use filenamelength_mod, only: filenamelength
 use errormessagelength_mod, only: errormessagelength
 use ereport_mod, only: ereport
 use rad_pcf, only: i_normal, i_err_fatal
 use realtype_rd, only: RealK
 use missing_data_mod, only: rmdi
 use yomhook,  only: lhook, dr_hook
 use parkind1, only: jprb, jpim

 implicit none


  ! Number of instances of the spectrum type (to allocate spectrum_array)
  integer (C_INT), intent(in), optional :: n_instances

  ! Spectral data:
  type(StrSpecData), intent(inout), target, optional :: spectrum
  character(len=*), intent(in), optional :: spectrum_name
  character(len=filenamelength), intent(in), optional :: spectral_file

  Logical (C_CHAR), intent(in), optional :: &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases

  real (RealK) (C_DOUBLE), intent(in), optional :: wavelength_blue

  CALL subroutine set_spectrum(n_instances, spectrum, spectrum_name, spectral_file, &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases, &
  wavelength_blue)

 end subroutine set_spectrum_wrap

END INTERFACE

END PROGRAM Interface_core
