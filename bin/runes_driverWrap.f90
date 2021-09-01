module runes_driverWrap

  USE interface_core
  use iso_c_binding

 contains

  subroutine runes_wrapper(n_profile, n_layer, spectrum_name , i_source, &
    p_layer, t_layer, mass, density, layer_heat_capacity, h2o , o3 , co2_mix_ratio, &
    n2o_mix_ratio ,ch4_mix_ratio ,o2_mix_ratio ,cos_zenith_angle, solar_irrad, &
    l_grey_albedo, grey_albedo, l_rayleigh, l_invert, heating_rate, flux_up, flux_down) bind(c)

    use def_spectrum, only: StrDiag 

 ! real(C_DOUBLE), parameter :: grav_acc = 9.80665
 ! real(C_DOUBLE), parameter :: r_gas_dry = 287.026
 ! real(C_DOUBLE), parameter :: cp_air_dry = 1.005e+03
 ! real(C_DOUBLE), parameter :: pi = 3.14159265358979323846
 ! real(C_DOUBLE), parameter :: seconds_per_day = 8.6400e+04

  integer(C_INT), intent(in) :: n_profile 
  integer(C_INT), intent(in) :: n_layer 
  type(StrDiag) :: sw_diag, lw_diag

  real(C_DOUBLE) :: heating_rate(n_profile, n_layer)
  real(C_DOUBLE) :: flux_up(n_profile, 0:n_layer)
  real(C_DOUBLE) :: flux_down(n_profile, 0:n_layer)
  real(C_DOUBLE) :: sw_heating_rate(n_profile, n_layer)
  real(C_DOUBLE) :: lw_heating_rate(n_profile, n_layer)
  real(C_DOUBLE) :: sw_flux_up(n_profile, 0:n_layer)
  real(C_DOUBLE) :: sw_flux_down(n_profile, 0:n_layer)
  real(C_DOUBLE) :: lw_flux_up(n_profile, 0:n_layer)
  real(C_DOUBLE) :: lw_flux_down(n_profile, 0:n_layer)

  ! Mid-latitude Summer McClatchey profile
  real(C_DOUBLE), intent(in) :: p_layer(n_profile, n_layer) 
  real(C_DOUBLE), intent(in) :: t_layer(n_profile, n_layer) 
  real(C_DOUBLE) :: p_level(n_profile, 0:n_layer) 
  real(C_DOUBLE) :: t_level(n_profile, 0:n_layer) 
  real(C_DOUBLE) :: t_ground(n_profile) 
  real(C_DOUBLE), intent(in) :: h2o(n_profile, n_layer) 
  real(C_DOUBLE), intent(in) :: o3(n_profile, n_layer) 

  real(C_DOUBLE), intent(in), value :: co2_mix_ratio 
  real(C_DOUBLE), intent(in), value :: ch4_mix_ratio 
  real(C_DOUBLE), intent(in), value :: n2o_mix_ratio 
  real(C_DOUBLE), intent(in), value :: o2_mix_ratio 

  real(C_DOUBLE), intent(in) :: cos_zenith_angle(n_profile) 
  real(C_DOUBLE), intent(in) :: solar_irrad(n_profile)
  real(C_DOUBLE) :: grey_albedo_sw 
  real(C_DOUBLE) :: grey_albedo_lw 

  real(C_DOUBLE), dimension(n_profile, n_layer) :: d_mass, density
  real(C_DOUBLE), dimension(n_profile, n_layer) :: layer_heat_capacity


  ! Read in spectral files at the beginning of a run
  call set_spectrum( &
   spectrum_name = 'sw', &
    !spectral_file='sp_sw_ga7', &
    l_all_gases = .true. )

  call set_spectrum( &
    spectrum_name = 'lw', &
    !spectral_file='sp_lw_ga7' , &
    l_all_gases = .true. )

  ! SW call (clear-sky, Rayleigh scattering and gas absorption only)
  
  ! diag%sw_diag%heating_rate => sw_heating_rate
  ! diag%sw_diag%flux_up => sw_flux_up
  ! diag%sw_diag%flux_down => sw_flux_down
  call runes( &
    n_profile = n_profile, &
    n_layer = n_layer, &
    !diag = sw_diag, &
    spectrum_name = 'sw', &
    i_source = ip_source_illuminate, &
    p_layer = p_layer, &
    t_layer = t_layer, &
    mass = d_mass, &
    density = density, &
    layer_heat_capacity = layer_heat_capacity, &
    h2o = h2o, &
    o3 = o3, &
    co2_mix_ratio = co2_mix_ratio, & ! These can all be 3D fields as well
    n2o_mix_ratio = n2o_mix_ratio, &
    ch4_mix_ratio = ch4_mix_ratio, &
    o2_mix_ratio = o2_mix_ratio, &
    cos_zenith_angle = cos_zenith_angle, &
    solar_irrad = solar_irrad, &
    l_grey_albedo = .true., & ! If false, need to specify albedos by band
    grey_albedo = grey_albedo_sw, &
    l_rayleigh = .true., &
    l_invert = .false.) ! If true, profiles can be supplied bottom-up

  ! LW call (clear-sky, gas absorption only)
  
  !diag%lw_diag%heating_rate => lw_heating_rate
  !diag%lw_diag%flux_up => lw_flux_up
  !diag%lw_diag%flux_down => lw_flux_down
  call runes( &
    n_profile = n_profile, &
    n_layer = n_layer, &
    !diag = lw_diag, &
    spectrum_name = 'lw', &
    i_source = ip_source_thermal, &
    p_layer = p_layer, &
    t_layer = t_layer, &
    t_level = t_level, &
    t_ground = t_ground, &
    mass = d_mass, &
    density = density, &
    layer_heat_capacity = layer_heat_capacity, &
    h2o = h2o, &
    o3 = o3, &
    co2_mix_ratio = co2_mix_ratio, &
    n2o_mix_ratio = n2o_mix_ratio, &
    ch4_mix_ratio = ch4_mix_ratio, &
    l_grey_albedo = .true., &
    grey_albedo = grey_albedo_lw, &
    l_invert = .false.)

   end subroutine runes_wrapper

end module runes_driverWrap
