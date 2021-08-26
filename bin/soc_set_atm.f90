! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps atmosphere subroutine

module soc_set_atm

 Use, intrinsic :: ISO_C_BINDING, Only: C_INT,C_BOOL,C_DOUBLE

 USE interface_core
 use def_atm,      only: StrAtm, allocate_atm
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
! use realtype_rd,  only: RealK

 
implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_ATM'
contains


subroutine set_atmwrap( n_profile, n_layer, &
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
  l_invert, l_debug, i_profile_debug) BIND(C,name='set_atm')


  ! use gas_list_pcf, only: ip_h2o, ip_co2, ip_o3, ip_n2o, ip_ch4, ip_o2, ip_so2, &
 ! ip_n2, ip_cfc11, ip_cfc12, ip_cfc113, ip_hcfc22, ip_hfc134a

  implicit none


  ! Atmospheric properties:
  type(StrAtm) :: atm

  ! Dimensions:
  type(StrDim)  :: dimen

  ! Spectral data:
  type(StrSpecData) :: spectrum

  Integer (C_INT), intent(in) :: n_profile
  !   Number of atmospheric profiles for radiation calculations
  Integer (C_INT), intent(in) :: n_layer
  !   Number of atmospheric layers for radiation calculations

  real(C_DOUBLE), intent(in), optional :: p_layer(:, :), p_layer_1d(:)
  !   Pressure at layer centres
  real(C_DOUBLE), intent(in), optional :: t_layer(:, :), t_layer_1d(:)
  !   Temperature at layer centres
  real(C_DOUBLE), intent(in), optional :: mass(:, :), mass_1d(:)
  !   Mass of layer (kg m-2)
  real(C_DOUBLE), intent(in), optional :: density(:, :), density_1d(:)
  !   Density of layer (kg m-3)
  real(C_DOUBLE), intent(in), optional :: p_level(:, 0:), p_level_1d(0:)
  !   Pressure at layer boundaries
  real(C_DOUBLE), intent(in), optional :: t_level(:, 0:), t_level_1d(0:)
  !   Temperature at layer boundaries
  real(C_DOUBLE), intent(in), optional :: r_layer(:, :), r_layer_1d(:)
  !   Radius (height from centre of planet) at layer centres
  real(C_DOUBLE), intent(in), optional :: r_level(:, 0:), r_level_1d(0:)
  !   Radius (height from centre of planet) at layer boundaries

  real(C_DOUBLE), intent(in), dimension(:, :), optional :: &
   h2o, co2, o3, n2o, ch4, o2, so2, n2, cfc11, cfc12, cfc113, hcfc22, hfc134a
  !   Full field mass mixing ratios

  real(C_DOUBLE), intent(in), dimension(:), optional :: &
   h2o_1d, co2_1d, o3_1d, n2o_1d, ch4_1d, o2_1d, so2_1d, n2_1d, cfc11_1d, &
   cfc12_1d, cfc113_1d, hcfc22_1d, hfc134a_1d
   !   1d mass mixing ratios

  real(C_DOUBLE), intent(in), optional :: &
   h2o_mix_ratio, co2_mix_ratio, o3_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
   o2_mix_ratio, so2_mix_ratio, n2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
   cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio
  !   Well mixed mass mixing ratios

  Logical, intent(in), optional :: &
   l_h2o_well_mixed, l_co2_well_mixed, l_o3_well_mixed, l_n2o_well_mixed, &
   l_ch4_well_mixed, l_o2_well_mixed, l_so2_well_mixed, l_n2_well_mixed, &
   l_cfc11_well_mixed, l_cfc12_well_mixed, l_cfc113_well_mixed, &
   l_hcfc22_well_mixed, l_hfc134a_well_mixed 
  !   Flag to use the well mixed ratios

  Logical, intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical

  Logical, intent(in), optional :: l_debug
  integer, intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

  Call set_atm(atm, dimen, spectrum, n_profile, n_layer, &
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

  
 end subroutine set_atmwrap

end module soc_set_atm