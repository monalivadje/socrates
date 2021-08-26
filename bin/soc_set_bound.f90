! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps bound subroutine

module soc_set_bound

 Use, intrinsic :: ISO_C_BINDING
 USE interface_core
 use def_bound,    only: StrBound, allocate_bound
  use def_control,  only: StrCtrl
  use def_dimen,    only: StrDim
  use def_spectrum, only: StrSpectData,StrSpecData
  use realtype_rd,  only: RealK



implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_BOUND'
contains

subroutine set_boundwrap( dimen, nd_band, n_profile, n_tile, &
  t_ground, cos_zenith_angle, solar_irrad, orog_corr, &
  l_grey_albedo, grey_albedo,albedo_diff, albedo_dir, &
   frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile,l_debug, i_profile_debug) BIND(C,name='set_bound')

  use rad_pcf,      only: &
  ip_solar, ip_infra_red, ip_surf_alb_diff, ip_surf_alb_dir

  implicit none


  ! Boundary properties:
  type(StrBound) :: bound

  ! Control options:
  type(StrCtrl) :: control
  

  ! Dimensions:
  type(StrDim),      intent(in)  :: dimen

  ! Spectral data:
  type(StrSpecData)  :: spect
   INTEGER (C_INT),intent(in) :: nd_band 


  integer(C_INT), intent(in) :: n_profile
  !   Number of atmospheric profiles for radiation calculations
  integer(C_INT), intent(in), optional :: n_tile
  !   Number of surface tiles for radiation calculations

  real(C_DOUBLE), intent(in), optional :: t_ground(n_profile)
  !   Effective radiative temperature over whole grid-box
  real(C_DOUBLE), intent(in), optional :: cos_zenith_angle(n_profile)
  !   Cosine of solar zenith angle
  real(C_DOUBLE), intent(in), optional :: solar_irrad(n_profile)
  !   Solar irradiance at top-of-atmosphere (mean over timestep)
  real(C_DOUBLE), intent(in), optional :: orog_corr(n_profile)
  !   Orographic correction factor

  Logical(C_BOOL), intent(in), optional :: l_grey_albedo
  !   Set a single grey albedo for the surface
  real(C_DOUBLE), intent(in), optional :: grey_albedo
  !   Grey surface albedo


  real(C_DOUBLE),intent(in) :: &
   albedo_diff(n_profile, nd_band), &
   albedo_dir(n_profile, nd_band)

  real(C_DOUBLE),intent(in)  :: &
   frac_tile(n_profile, dimen%nd_tile), &
   albedo_diff_tile(n_profile, dimen%nd_tile, nd_band), &
   albedo_dir_tile(n_profile, dimen%nd_tile, nd_band), &
   t_tile(n_profile, dimen%nd_tile)

  !type(StrSpecData)  :: sp 
  Logical(C_BOOL), intent(in), optional :: l_debug
  integer (C_INT), intent(in), optional :: i_profile_debug
  !   Options for outputting debugging information

 
  Call set_bound(bound, control, dimen, spect, &
   n_profile, n_tile, &
   t_ground, cos_zenith_angle, solar_irrad, orog_corr, &
   l_grey_albedo, grey_albedo, albedo_diff, albedo_dir, &
   frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile, &
   l_debug, i_profile_debug)

 
 end subroutine set_boundwrap

end module soc_set_bound