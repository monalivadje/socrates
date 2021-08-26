! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Module to wrap runes.


module soc_runes

 Use, intrinsic :: ISO_C_BINDING, Only: C_INT,C_BOOL,C_DOUBLE
 USE interface_core
  use def_spectrum, only: StrSpecData
  use def_mcica,    only: StrMcica
 use rad_pcf, only: &
  ip_source_illuminate                      => ip_solar, &
  ip_source_thermal                         => ip_infra_red, &
  ip_cloud_representation_off               => ip_cloud_off, &
  ip_cloud_representation_ice_water         => ip_cloud_ice_water, &
  ip_cloud_representation_combine_ice_water => ip_cloud_combine_ice_water, &
  ip_cloud_representation_csiw              => ip_cloud_csiw, &
  ip_cloud_representation_split_ice_water   => ip_cloud_split_ice_water, &
  ip_overlap_max_random                     => ip_max_rand, &
  ip_overlap_random                         => ip_rand, &
  ip_overlap_exponential_random             => ip_exponential_rand, &
  ip_inhom_homogeneous                      => ip_homogeneous, &
  ip_inhom_scaling                          => ip_scaling, &
  ip_inhom_mcica                            => ip_mcica, &
  ip_inhom_cairns                           => ip_cairns, &
  ip_inhom_tripleclouds_2019                => ip_tripleclouds_2019, &
  ip_droplet_re_external                    => ip_re_external, &
  ip_droplet_re_liu                         => ip_re_liu, &
  ip_droplet_re_default                     => ip_re_default

 implicit none
 character(len=*), parameter, private :: ModuleName = 'SOC_RUNES'

 contains


   subroutine runeswrap(n_profile, n_layer, &
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
   liq_conv_dim_diag_1d, ice_conv_dim_diag_1d) BIND(C,name='runes')


  Use, intrinsic :: ISO_C_BINDING , only : C_INT, C_DOUBLE, C_BOOL 
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
  use realtype_rd, only: RealK
  use ereport_mod,  only: ereport
  use errormessagelength_mod, only: errormessagelength
  use rad_pcf, only: i_normal, i_err_fatal
  Use, intrinsic :: ISO_C_BINDING, Only: C_INT,C_BOOL,C_DOUBLE
  use filenamelength_mod, only: filenamelength

 implicit none

  !character(len=*), :: spectrum_name 
  character(len=filenamelength) :: spectrum_name

  ! Spectral data:
  type (StrSpecData) :: spectrum
    
  ! Mcica data
   type (StrMcica) :: mcica_data

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

  ! character(C_CHAR),intent(in), optional :: spectrum_name 

  real(C_DOUBLE), intent(in), optional :: p_layer(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: p_layer_1d(n_layer)
  !   Pressure at layer centres
  real(C_DOUBLE), intent(in), optional :: t_layer(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: t_layer_1d(n_layer)
  !   Temperature at layer centres
  real(C_DOUBLE), intent(in), optional :: t_level(n_profile, 0:n_layer)
  real(C_DOUBLE), intent(in), optional :: t_level_1d(0:n_layer)
  !   Temperature at layer boundaries
  real(C_DOUBLE), intent(in), optional :: mass(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: mass_1d(n_layer)
  !   Mass of layer (kg m-2)
  real(C_DOUBLE), intent(in), optional :: density(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: density_1d(n_layer)   
  !   Density of layer (kg m-3)
  real(C_DOUBLE), intent(in), optional :: h2o(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: h2o_1d(n_layer)
  !   Mass mixing ratio of water vapour
  real(C_DOUBLE), intent(in), optional :: o3(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: o3_1d(n_layer)
  !   Mass mixing ratio of ozone

  real(C_DOUBLE), intent(in), optional :: &
   co2_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
   o2_mix_ratio, so2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
   cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio
  !   Trace gas mass mixing ratios

  real(C_DOUBLE), intent(in), optional :: t_ground(n_profile)
  !   Effective radiative temperature over whole grid-box
  real(C_DOUBLE), intent(in), optional :: cos_zenith_angle(n_profile)
  !   Cosine of solar zenith angle
  real(C_DOUBLE), intent(in), optional :: solar_irrad(n_profile)
  !   Solar irradiance at top-of-atmosphere (mean over timestep)
  real(C_DOUBLE), intent(in), optional :: orog_corr(n_profile)
  !   Orographic correction factor

  Logical , intent(in), optional :: l_grey_albedo
  !   Set a single grey albedo / emissivity for the surface
  real(C_DOUBLE), intent(in), optional :: grey_albedo
  !   Grey surface albedo

  real(C_DOUBLE) , intent(in), optional :: albedo_diff(:, :)
  !   Spectral diffuse albedo (n_profile, n_band)
  real(C_DOUBLE), intent(in), optional :: albedo_dir(:, :)
  !   Spectral direct albedo (n_profile, n_band)

  Logical , intent(in), optional :: l_tile
  !   Use tiled surface properties
  real(C_DOUBLE), intent(in), optional :: frac_tile(:, :)
  !   Tile fractions (n_profile, n_tile)
  real(C_DOUBLE), intent(in), optional :: albedo_diff_tile(:, :, :)
  !   Diffuse tile albedo (n_profile, n_tile, n_band)
  real(C_DOUBLE), intent(in), optional :: albedo_dir_tile(:, :, :)
  !   Direct tile albedo (n_profile, n_tile, n_band)
  real(C_DOUBLE), intent(in), optional :: t_tile(:, :)
  !   Tile temperatures (n_profile, n_tile)

  real(C_DOUBLE), intent(in), dimension (n_profile, n_layer), optional :: &
   cloud_frac, conv_frac, &
   liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
   liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
   liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
   liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
   liq_nc, liq_conv_nc
  real(C_DOUBLE), intent(in), dimension (n_layer), optional :: &
   cloud_frac_1d, conv_frac_1d, &
   liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
   liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
   liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
   liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
   liq_nc_1d, liq_conv_nc_1d
  !   Liquid and ice cloud fractions, gridbox mean mixing ratios,
  !   effective dimensions, relative standard deviation of condensate,
  !   and number concentration

  real(C_DOUBLE), intent(in), optional :: cloud_vertical_decorr
  !   Decorrelation pressure scale for cloud vertical overlap
  real(C_DOUBLE), intent(in), optional :: conv_vertical_decorr
  !   Decorrelation pressure scale for convective cloud vertical overlap
  real(C_DOUBLE), intent(in), optional :: cloud_horizontal_rsd
  !   Relative standard deviation of sub-grid cloud condensate

  real(C_DOUBLE), intent(in), optional :: layer_heat_capacity(n_profile, n_layer)
  real(C_DOUBLE), intent(in), optional :: layer_heat_capacity_1d(n_layer)
  !   Heat capacity of layer

  Integer(C_INT), intent(in), optional :: i_source
  !   Select source of radiation
  Integer(C_INT), intent(in), optional :: &
  i_cloud_representation, i_overlap, i_inhom, &
  i_mcica_sampling, i_st_water, i_st_ice, i_cnv_water, i_cnv_ice, i_drop_re
  !   Select treatment of cloud
  Integer(C_INT), intent(in), optional :: rand_seed(n_profile)
  !   Random seed for cloud generator

  Logical , intent(in), optional :: l_rayleigh
  !   Include Rayleigh scattering
  Logical  , intent(in), optional :: l_mixing_ratio
  !   Assume mass mixing ratios are with respect to dry mass
  Logical , intent(in), optional :: l_aerosol_mode
  !   Include aerosol optical properties specified by mode

  Logical , intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical

  Logical  , intent(in), optional :: l_debug
  integer, intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information


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

  Integer(C_INT) :: id_spec, id_mcica
  !   Loop variables
  logical(C_BOOL) :: l_blue_flux_surf
  !   Output blue fluxes if requested

  Integer(C_INT) :: ierr = i_normal
  character (len=errormessagelength) :: cmessage
  character(len=*), parameter :: RoutineName = 'RUNES'

  CALL runes(n_profile, n_layer, spectrum,spectrum_name,mcica_data,&
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


 end subroutine runeswrap

end module soc_runes