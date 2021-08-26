! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps socrates Cloud subroutine

module soc_set_cld

Use, intrinsic :: ISO_C_BINDING
USE interface_core
use def_cld,      only: StrCld, allocate_cld, allocate_cld_prsc
  use def_control,  only: StrCtrl
  use def_dimen,    only: StrDim
  use def_spectrum, only: StrSpecData
  use def_atm,      only: StrAtm


implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_CLD'
contains


 subroutine set_cldwrap( cloud_frac, conv_frac, &
  liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
  liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
  liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
  cloud_frac_1d, conv_frac_1d, &
  liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
  liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
  liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
  cloud_vertical_decorr, conv_vertical_decorr, cloud_horizontal_rsd, &
  l_invert, l_debug, i_profile_debug) BIND(C,name='set_cld')


  implicit none


  ! Cloud properties:
  type(StrCld) :: cld

  ! Control options:
  type(StrCtrl)  :: control

  ! Dimensions:
  type(StrDim)  :: dimen

  ! Spectral data:
  type(StrSpecData)  :: spectrum

  ! Atmospheric properties:
  type(StrAtm)  :: atm

  real(C_DOUBLE), intent(in), dimension(:, :), optional :: &
   cloud_frac, conv_frac, &
   liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
   liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
   liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd
  !   Liquid and ice cloud fractions, gridbox mean mixing ratios,
  !   and relative standard deviation of condensate

  real(C_DOUBLE), intent(in), dimension(:), optional :: &
   cloud_frac_1d, conv_frac_1d, &
   liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
   liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
   liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d
  !   Liquid and ice cloud fractions, gridbox mean mixing ratios,
  !   and relative standard deviation of condensate input as 1d fields

  real(C_DOUBLE), intent(in), optional :: cloud_vertical_decorr
  !   Decorrelation pressure scale for cloud vertical overlap
  real(C_DOUBLE), intent(in), optional :: conv_vertical_decorr
  !   Decorrelation pressure scale for convective cloud vertical overlap
  real(C_DOUBLE), intent(in), optional :: cloud_horizontal_rsd
  !   Relative standard deviation of sub-grid cloud condensate

  Logical, intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical

  Logical, intent(in), optional :: l_debug
  integer, intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information


  Call set_cld(cld, control, dimen, spectrum, atm, &
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

 end subroutine set_cldwrap 

end module soc_set_cld