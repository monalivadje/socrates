! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps Cloud dimension subroutine


module soc_set_cld_dim

Use, intrinsic :: ISO_C_BINDING
USE interface_core
use def_cld,      only: StrCld
use def_control,  only: StrCtrl
use def_dimen,    only: StrDim
use def_spectrum, only: StrSpecData
use def_atm,      only: StrAtm


implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_CLD_DIM'
contains

 subroutine set_cld_dimwrap( liq_nc, liq_conv_nc, &
  liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
  liq_nc_1d, liq_conv_nc_1d, &
  liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
  l_invert, l_debug, i_profile_debug) BIND(C,name='set_cld_dim')



 ! Use, intrinsic :: ISO_C_BINDING 
 ! use def_cld,      only: StrCld
 ! use def_control,  only: StrCtrl
 ! use def_dimen,    only: StrDim
 ! use def_spectrum, only: StrSpecData
 ! use def_atm,      only: StrAtm
  !use realtype_rd,  only: RealK
  !use rad_pcf,      only: &
 ! ip_cloud_split_homogen, ip_cloud_split_ice_water, &
 ! ip_clcmp_st_water, ip_clcmp_st_ice, ip_clcmp_cnv_water, ip_clcmp_cnv_ice, &
 ! ip_re_external, &
 ! i_normal, i_err_fatal
 ! use ereport_mod,  only: ereport
  !use errormessagelength_mod, only: errormessagelength

  implicit none


  ! Cloud properties:
  type(StrCld) :: cld

  ! Control options:
  type(StrCtrl) :: control

  ! Dimensions:
  type(StrDim)  :: dimen

  ! Spectral data:
  type(StrSpecData)  :: spectrum

  ! Atmospheric properties:
  type(StrAtm)  :: atm

  real(C_DOUBLE), intent(in), dimension(:, :), optional :: &
   liq_nc, liq_conv_nc, &
   liq_dim, ice_dim, liq_conv_dim, ice_conv_dim
  real(C_DOUBLE), intent(in), dimension(:), optional :: &
   liq_nc_1d, liq_conv_nc_1d, &
   liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d
  !   Liquid number concentration, liquid and ice effective dimensions

  Logical, intent(in), optional :: l_invert
  !   Flag to invert fields in the vertical

  logical, intent(in), optional :: l_debug
  integer(C_INT), intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

  Call set_cld_dim(cld, control, dimen, spectrum, atm, &
  liq_nc, liq_conv_nc, &
  liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
  liq_nc_1d, liq_conv_nc_1d, &
  liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
  l_invert, l_debug, i_profile_debug)

 end subroutine set_cld_dimwrap

end module soc_set_cld_dim