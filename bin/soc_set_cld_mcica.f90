! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps Cloud mcica subroutine


module soc_set_cld_mcica

!use socrates_cloud_gen
Use, intrinsic :: ISO_C_BINDING
use def_cld,      only: StrCld, allocate_cld_mcica
use def_mcica,    only: StrMcica, ip_mcica_full_sampling, &
ip_mcica_single_sampling, ip_mcica_optimal_sampling
 use def_control,  only: StrCtrl
 use def_dimen,    only: StrDim
 use def_spectrum, only: StrSpecData
 use def_atm,      only: StrAtm

implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_SET_CLD_MCICA'
contains


 subroutine set_cld_mcicawrap(dimen, rand_seed) BIND(C,name='set_cld_mcica')

 
  ! Use, intrinsic :: ISO_C_BINDING 
  ! use def_cld,      only: StrCld, allocate_cld_mcica
  ! use def_mcica,    only: StrMcica, ip_mcica_full_sampling, &
  ! ip_mcica_single_sampling, ip_mcica_optimal_sampling
  ! use def_control,  only: StrCtrl
  ! use def_dimen,    only: StrDim
  ! use def_spectrum, only: StrSpecData
  ! use def_atm,      only: StrAtm
  ! use realtype_rd,  only: RealK
   use rad_pcf,      only: &
   ip_cloud_off, ip_cloud_mcica, ip_solar, &
   i_normal, i_err_fatal
  use ereport_mod,  only: ereport
   use errormessagelength_mod, only: errormessagelength


   implicit none


  ! Cloud properties:
  type(StrCld) :: cld

  ! Mcica data:
  type(StrMcica) :: mcica_data

  ! Control options:
  type(StrCtrl) :: control

  
  type(StrDim), intent(in) :: dimen

  ! Spectral data:
  type(StrSpecData) :: spectrum

  ! Atmospheric properties:
  type(StrAtm) :: atm

  integer (C_INT), intent(in), optional :: rand_seed(:)
  !   Random seed for cloud generator

  
  real(C_DOUBLE), dimension(dimen%nd_profile, dimen%id_cloud_top:dimen%nd_layer) :: &
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

  Call set_cld_mcica(cld, mcica_data, control, dimen, spectrum, atm, &
  rand_seed)

 end subroutine set_cld_mcicawrap

end module soc_set_cld_mcica