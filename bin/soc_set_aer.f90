! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps socrates aerosol


module soc_set_aer

Use, intrinsic :: ISO_C_BINDING
USE interface_core
 use def_aer,      only: StrAer, allocate_aer, allocate_aer_prsc
 use def_control,  only: StrCtrl
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
! use realtype_rd,  only: RealK



implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_AER'
contains

 subroutine set_aerwrap( n_profile, n_layer, n_aer_mode, &
   aer_mix_ratio, aer_absorption, aer_scattering, aer_asymmetry) BIND(C,name='set_aer')

  implicit none


  ! Aerosol properties:
  type(StrAer) :: aer

  ! Control options:
  type(StrCtrl) :: control

  ! Dimensions:
  type(StrDim) :: dimen

  ! Spectral data:
  type(StrSpecData) :: spectrum

  Integer (C_INT), intent(in) :: n_profile
  Integer (C_INT), intent(in) :: n_layer

  Integer (C_INT), intent(in), optional :: n_aer_mode
  !   Number of aerosol modes

  real(C_DOUBLE), intent(in), optional :: aer_mix_ratio(:, :, :)
  !   Mass-mixing ratio (n_profile, n_layer, n_mode)
  real(C_DOUBLE), intent(in), optional :: aer_absorption(:, :, :, :)
  !   Aerosol absorption (n_profile, n_layer, n_mode, n_band)
  real(C_DOUBLE), intent(in), optional :: aer_scattering(:, :, :, :)
  !   Aerosol scattering (n_profile, n_layer, n_mode, n_band)
  real(C_DOUBLE), intent(in), optional :: aer_asymmetry(:, :, :, :)
  !   Aerosol asymmetry (n_profile, n_layer, n_mode, n_band)

  call set_aer(aer, control, dimen, spectrum, &
   n_profile, n_layer, n_aer_mode, &
   aer_mix_ratio, aer_absorption, aer_scattering, aer_asymmetry) 


 end subroutine set_aerwrap

end module soc_set_aer