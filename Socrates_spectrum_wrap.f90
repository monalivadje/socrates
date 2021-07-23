module Socrates_set_spectrum_wrap

use def_spectrum, only: StrSpecData
use def_mcica, only: StrMcica
Use, intrinsic :: ISO_C_BINDING

!implicit none
private
public :: set_spectrum, compress_spectrum, set_weight_blue, get_spectrum, &
          spectrum_array_name, spectrum_array, &
          set_mcica, mcica_spectrum_name, mcica_data_array

integer, parameter :: specnamelength = 64
character(len=specnamelength), allocatable, save :: spectrum_array_name(:)
character(len=specnamelength), allocatable, save :: mcica_spectrum_name(:, :)
type(StrSpecData), allocatable, target, save :: spectrum_array(:)
type(StrMcica), allocatable, target, save :: mcica_data_array(:)

character(len=*), parameter :: ModuleName='SOCRATES_SET_SPECTRUM'

contains


 subroutine set_spectrum_wrap(n_instances, spectrum, spectrum_name, spectral_file, &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases, &
  wavelength_blue) 


   Use, intrinsic :: ISO_C_BINDING 
   use filenamelength_mod, only: filenamelength
   use errormessagelength_mod, only: errormessagelength
   use ereport_mod, only: ereport
   use rad_pcf, only: i_normal, i_err_fatal
   use realtype_rd, only: RealK
   use missing_data_mod, only: rmdi
   use yomhook,  only: lhook, dr_hook
   use parkind1, only: jprb, jpim
   use def_spectrum

 !  implicit none


  ! Number of instances of the spectrum type (to allocate spectrum_array)
  integer (C_INT), intent(in), optional :: n_instances

  ! Spectral data:
  type(StrSpecData), intent(inout), target, optional :: spectrum
  character(len=*), intent(in), optional :: spectrum_name
  character(len=filenamelength), intent(in), optional :: spectral_file

  Logical (C_BOOL), intent(in), optional :: &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases

  real (C_DOUBLE), intent(in), optional :: wavelength_blue

  Call set_spectrum(n_instances, spectrum, spectrum_name, spectral_file, &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases, &
   wavelength_blue) 
 
 end subroutine set_spectrum_wrap

 subroutine compress_spectrum_wrap(spec, &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases)

   use gas_list_pcf, only: &
    ip_h2o, ip_co2, ip_o3, ip_o2, ip_n2o, ip_ch4, ip_so2, ip_cfc11, ip_cfc12, &
    ip_cfc113, ip_cfc114, ip_hcfc22, ip_hfc125, ip_hfc134a, ip_co, ip_nh3, &
    ip_tio, ip_vo, ip_h2, ip_he, ip_na, ip_k, ip_li, ip_rb, ip_cs

!  implicit none

  type(StrSpecData), intent(inout) :: spec

  logical(C_BOOL), intent(in), optional :: &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases

  integer(C_INT) :: i, j, n_band_absorb
  logical(C_BOOL) :: l_retain_absorb(spec%gas%n_absorb)
  logical(C_BOOL) :: l_retain_all

  Call compress_spectrum(spec, &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases)


 end subroutine compress_spectrum_wrap 

 subroutine set_weight_blue_wrap(spec, wavelength_blue)

  !use realtype_rd, only: RealK

  !implicit none

  type(StrSpecData), intent(inout) :: spec
  real (C_DOUBLE), intent(in), optional :: wavelength_blue
  
  Call set_weight_blue(spec, wavelength_blue)

 end subroutine set_weight_blue_wrap

 subroutine get_spectrum_wrap(spectrum_name, spectrum, &
   n_band, wavelength_short, wavelength_long, weight_blue)

   use realtype_rd, only: RealK
   use errormessagelength_mod, only: errormessagelength
   use ereport_mod, only: ereport
   use rad_pcf, only: i_normal, i_err_fatal

   !implicit none

  character(len=*), intent(in) :: spectrum_name
  type (StrSpecData), intent(out), optional :: spectrum

  integer(C_INT), optional, intent(out) :: n_band
  real (RealK), allocatable, optional, intent(out) :: wavelength_short(:)
  real (RealK), allocatable, optional, intent(out) :: wavelength_long(:)
  real (RealK), allocatable, optional, intent(out) :: weight_blue(:)

  Call get_spectrum(spectrum_name, spectrum, &
   n_band, wavelength_short, wavelength_long, weight_blue)

 end subroutine get_spectrum_wrap

 subroutine set_mcica_wrap(mcica_data_file, sw_spectrum_name, lw_spectrum_name)

   use def_mcica, only: read_mcica_data
   use errormessagelength_mod, only: errormessagelength
   use ereport_mod, only: ereport
   use rad_pcf, only: i_normal, i_err_fatal
   use yomhook,  only: lhook, dr_hook
   use parkind1, only: jprb, jpim

   !implicit none


 ! Spectral data:
  character(len=*), intent(in) :: mcica_data_file
  character(len=*), intent(in), optional :: sw_spectrum_name, lw_spectrum_name

  Call set_mcica(mcica_data_file, sw_spectrum_name, lw_spectrum_name)

 end subroutine set_mcica_wrap

end module Socrates_set_spectrum_wrap
