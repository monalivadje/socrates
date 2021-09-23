program runes_driver

  USE interface_core
  use runes_driverwrap
  
  Type StrDiag
 
  integer :: n_profile 
  integer :: n_layer 
  
  real(RealK), dimension(n_profile , n_layer ), pointer :: heating_rate(:, :)
  real(RealK), dimension(n_profile , n_layer ), pointer :: flux_up(:,:)
  real(RealK), dimension(n_profile , n_layer ), pointer :: flux_down(:,:)
  

 END TYPE StrDiag 

  real(RealK), parameter :: grav_acc = 9.80665
  real(RealK), parameter :: r_gas_dry = 287.026
  real(RealK), parameter :: cp_air_dry = 1.005e+03
  real(RealK), parameter :: pi = 3.14159265358979323846
  real(RealK), parameter :: seconds_per_day = 8.6400e+04

  integer, parameter :: n_profile = 1
  integer, parameter :: n_layer = 32
  type(StrDiag), pointer :: sw_diag, lw_diag
  real(RealK), target :: sw_heating_rate(1, 32)
  real(RealK), target :: lw_heating_rate(1, 32)
  real(RealK), target :: sw_flux_up(1, 0:32)
  real(RealK), target :: sw_flux_down(1, 0:32)
  real(RealK), target :: lw_flux_up(1, 0:32)
  real(RealK), target :: lw_flux_down(1, 0:32)

  ! Mid-latitude Summer McClatchey profile
  real(RealK) :: p_layer(1, 32) = reshape( (/ &
    0.337000E+01, 0.509050E+02, 0.135550E+03, 0.254500E+03, &
    0.492500E+03, 0.986000E+03, 0.204500E+04, 0.299500E+04, &
    0.349000E+04, 0.406500E+04, 0.473500E+04, 0.552500E+04, &
    0.645000E+04, 0.753500E+04, 0.881000E+04, 0.103000E+05, &
    0.120500E+05, 0.141500E+05, 0.166000E+05, 0.194000E+05, &
    0.226000E+05, 0.262000E+05, 0.302500E+05, 0.348000E+05, &
    0.399000E+05, 0.456500E+05, 0.520500E+05, 0.591000E+05, &
    0.669000E+05, 0.756000E+05, 0.852000E+05, 0.957500E+05 /), &
    (/ 1, 32 /) )

  real(RealK) :: t_layer(1, 32) = reshape( (/ &
    0.216982E+03, 0.262328E+03, 0.272545E+03, 0.263059E+03, &
    0.250428E+03, 0.238550E+03, 0.228094E+03, 0.223481E+03, &
    0.222481E+03, 0.220962E+03, 0.219481E+03, 0.218481E+03, &
    0.217481E+03, 0.216481E+03, 0.216000E+03, 0.216000E+03, &
    0.216000E+03, 0.216000E+03, 0.216000E+03, 0.219116E+03, &
    0.225632E+03, 0.232109E+03, 0.238624E+03, 0.245104E+03, &
    0.251619E+03, 0.258100E+03, 0.264097E+03, 0.270094E+03, &
    0.276092E+03, 0.282091E+03, 0.287573E+03, 0.292058E+03 /), &
    (/ 1, 32 /) )

  real(RealK) :: p_level(1, 0:32) = reshape( (/ &
    0.300000E-01, 0.671000E+01, 0.951000E+02, 0.176000E+03, &
    0.333000E+03, 0.652000E+03, 0.132000E+04, 0.277000E+04, &
    0.322000E+04, 0.376000E+04, 0.437000E+04, 0.510000E+04, &
    0.595000E+04, 0.695000E+04, 0.812000E+04, 0.950000E+04, &
    0.111000E+05, 0.130000E+05, 0.153000E+05, 0.179000E+05, &
    0.209000E+05, 0.243000E+05, 0.281000E+05, 0.324000E+05, &
    0.372000E+05, 0.426000E+05, 0.487000E+05, 0.554000E+05, &
    0.628000E+05, 0.710000E+05, 0.802000E+05, 0.902000E+05, &
    0.101300E+06 /), (/ 1, 32+1 /) )

  real(RealK) :: t_level(1, 0:32) = reshape( (/ &
    0.210000E+03, 0.218000E+03, 0.276000E+03, 0.270000E+03, &
    0.258000E+03, 0.245000E+03, 0.234000E+03, 0.224000E+03, &
    0.223000E+03, 0.222000E+03, 0.220000E+03, 0.219000E+03, &
    0.218000E+03, 0.217000E+03, 0.216000E+03, 0.216000E+03, &
    0.216000E+03, 0.216000E+03, 0.216000E+03, 0.216000E+03, &
    0.222000E+03, 0.229000E+03, 0.235000E+03, 0.242000E+03, &
    0.248000E+03, 0.255000E+03, 0.261000E+03, 0.267000E+03, &
    0.273000E+03, 0.279000E+03, 0.285000E+03, 0.290000E+03, &
    0.294000E+03 /), (/ 1, 32+1 /) )

  real(RealK) :: t_ground(1) = 294.0

  real(RealK) :: h2o(1, 32) = reshape( (/ &
    0.399688E-05, 0.399530E-05, 0.399851E-05, 0.399700E-05, &
    0.399963E-05, 0.400241E-05, 0.400722E-05, 0.400994E-05, &
    0.400705E-05, 0.400353E-05, 0.399929E-05, 0.399791E-05, &
    0.399939E-05, 0.400000E-05, 0.400000E-05, 0.400058E-05, &
    0.400152E-05, 0.402072E-05, 0.485647E-05, 0.109264E-04, &
    0.349482E-04, 0.974304E-04, 0.199405E-03, 0.321272E-03, &
    0.509681E-03, 0.777969E-03, 0.114820E-02, 0.182544E-02, &
    0.305008E-02, 0.485372E-02, 0.722366E-02, 0.101064E-01 /), &
    (/ 1, 32 /) )

  real(RealK) :: o3(1, 32) = reshape( (/ &
    0.606562E-06, 0.252165E-05, 0.469047E-05, 0.748127E-05, &
    0.957770E-05, 0.100812E-04, 0.814088E-05, 0.664711E-05, &
    0.603987E-05, 0.546986E-05, 0.480064E-05, 0.397211E-05, &
    0.319003E-05, 0.246208E-05, 0.181795E-05, 0.135296E-05, &
    0.102925E-05, 0.808670E-06, 0.612577E-06, 0.434212E-06, &
    0.328720E-06, 0.252055E-06, 0.198937E-06, 0.166297E-06, &
    0.139094E-06, 0.116418E-06, 0.981116E-07, 0.850660E-07, &
    0.743462E-07, 0.649675E-07, 0.577062E-07, 0.520021E-07 /), &
    (/ 1, 32 /) )

  real(RealK) :: co2_mix_ratio = 6.0e-4
  real(RealK) :: ch4_mix_ratio = 9.637e-7
  real(RealK) :: n2o_mix_ratio = 4.7255e-7
  real(RealK) :: o2_mix_ratio = 0.23139

  real(RealK) :: cos_zenith_angle(1) = cos(45.0*pi/180.0)
  real(RealK) :: solar_irrad(1) = 1361.0
  real(RealK) :: grey_albedo_sw = 0.06
  real(RealK) :: grey_albedo_lw = 0.0

  real(RealK), dimension(1, 32) :: d_mass, density
  real(RealK), dimension(1, 32) :: layer_heat_capacity

  integer :: i, l

  do i=1, n_layer
    do l=1, n_profile
      d_mass(l, i) = (p_level(l, i)-p_level(l, i-1))/grav_acc
      density(l, i) = p_layer(l, i)/(r_gas_dry*t_layer(l, i))
      layer_heat_capacity(l, i) = d_mass(l, i)*cp_air_dry
    end do
  end do

  ! Read in spectral files at the beginning of a run
  call set_spectrum( &
    !spectrum_name = 'sw', &
    !spectral_file = 'sp_sw_ga7', &
    l_all_gases = .true. )

  call set_spectrum( &
    !spectrum_name = 'lw', &
    !spectral_file = 'sp_lw_ga7', &
    l_all_gases = .true. )

  ! SW call (clear-sky, Rayleigh scattering and gas absorption only)
  sw_diag%heating_rate => sw_heating_rate
  sw_diag%flux_up => sw_flux_up
  sw_diag%flux_down => sw_flux_down
  call runes( &
    n_profile = n_profile, &
    n_layer = n_layer, &
   ! diag = sw_diag, &
    !spectrum_name = 'sw', &
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

  write(*,'(a)') &
    'SW: flux down (Wm-2) | flux up (Wm-2) | heating rate (K/day)'
  do i=1, n_layer
    write(*, '(2x, 3f17.8)') sw_flux_down(1, i), sw_flux_up(1, i), &
      sw_heating_rate(1, i)*seconds_per_day
  end do

  ! LW call (clear-sky, gas absorption only)
  lw_diag%heating_rate => lw_heating_rate
  lw_diag%flux_up => lw_flux_up
  lw_diag%flux_down => lw_flux_down
  call runes( &
    n_profile = n_profile, &
    n_layer = n_layer, &
    !diag = lw_diag, &
    !spectrum_name = 'lw', &
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

  write(*,*)
  write(*,'(a)') &
    'LW: flux down (Wm-2) | flux up (Wm-2) | heating rate (K/day)'
  do i=1, n_layer
    write(*, '(2x, 3f17.8)') lw_flux_down(1, i), lw_flux_up(1, i), &
      lw_heating_rate(1, i)*seconds_per_day
  end do

end program runes_driver
