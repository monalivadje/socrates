! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps spectrum subroutine

module soc_set_spectrum

USE interface_core
use def_spectrum, only: StrSpecData, StrSpectData
use def_mcica, only: StrMcica
Use, intrinsic :: ISO_C_BINDING

!implicit none
private
public :: set_spectrum, compress_spectrum, set_weight_blue, get_spectrum, &
          spectrum_array_name, spectrum_array, &
          set_mcica, mcica_spectrum_name, mcica_data_array

integer, parameter :: specnamelength = 64
character(len=*), parameter :: ModuleName='SOCRATES_SET_SPECTRUM'

 contains


 subroutine set_spectrumWrap(n_instances, &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases,wavelength_blue) BIND(C,name='set_spectrum')


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

 
  ! Number of instances of the spectrum type (to allocate spectrum_array)
  integer (C_INT), intent(in), optional :: n_instances

  type(StrSpecData) :: spectrum
    real(C_DOUBLE), intent(in), optional :: wavelength_blue

  ! Spectral data:
  type(StrSpectData) :: sp
  integer(C_INT) :: len
  character(len=filenamelength, kind=C_CHAR ) :: spectrum_name
  character(len=filenamelength, kind=C_CHAR) :: spectral_file
   

  Logical , intent(in), optional :: &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases


  Call set_spectrum(n_instances, spectrum, spectrum_name, spectral_file, &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases, &
   wavelength_blue) 
 
 end subroutine set_spectrumWrap

 subroutine compress_spectrumWrap(l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases) BIND(C,name='compress_spectrum')

   !USE interface_core
   use gas_list_pcf, only: &
    ip_h2o, ip_co2, ip_o3, ip_o2, ip_n2o, ip_ch4, ip_so2, ip_cfc11, ip_cfc12, &
    ip_cfc113, ip_cfc114, ip_hcfc22, ip_hfc125, ip_hfc134a, ip_co, ip_nh3, &
    ip_tio, ip_vo, ip_h2, ip_he, ip_na, ip_k, ip_li, ip_rb, ip_cs

!  implicit none

  type(StrSpecData) :: spec

  logical , intent(in), optional :: &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases

  
  Call compress_spectrum(spec, &
   l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
   l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
   l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, l_all_gases)


 end subroutine compress_spectrumWrap 

 subroutine set_weight_blueWrap(wavelength_blue) BIND(C,name='weight_spectrum')

  type(StrSpecData) :: spec
  real(C_DOUBLE), intent(in), optional :: wavelength_blue
  
  Call set_weight_blue(spec, wavelength_blue)

 end subroutine set_weight_blueWrap

 subroutine get_spectrumWrap( n_band )BIND(C,name='get_spectrum')

   use realtype_rd, only: RealK
   use errormessagelength_mod, only: errormessagelength
   use ereport_mod, only: ereport
   use rad_pcf, only: i_normal, i_err_fatal
   use filenamelength_mod, only: filenamelength

   !implicit none

  character(len=filenamelength) :: spectrum_name
  type (StrSpecData):: spectrum

  integer(C_INT), optional, intent(out) :: n_band
  real (RealK),allocatable :: wavelength_short(:)
  real (RealK),allocatable :: wavelength_long(:)
  real (RealK),allocatable :: weight_blue(:)

  Call get_spectrum(spectrum_name, spectrum, &
   n_band, wavelength_short, wavelength_long, weight_blue)

 end subroutine get_spectrumWrap

 subroutine set_mcicaWrap(mcica_data_file,sw_spectrum_name, lw_spectrum_name) BIND(C,name='set_mcica_spectrum')


   !USE interface_core
   use def_mcica, only: read_mcica_data
   use errormessagelength_mod, only: errormessagelength
   use ereport_mod, only: ereport
   use rad_pcf, only: i_normal, i_err_fatal
   use yomhook,  only: lhook, dr_hook
   use parkind1, only: jprb, jpim

   !implicit none


 ! Spectral data:

  !character(len=*, kind=C_CHAR), intent(out) :: mcica_data_file
  !character(len=*, kind=C_CHAR), intent(out) :: sw_spectrum_name, lw_spectrum_name

   character(C_CHAR), intent(out) :: mcica_data_file
   character(C_CHAR), intent(out) :: sw_spectrum_name, lw_spectrum_name

  Call set_mcica(mcica_data_file, sw_spectrum_name, lw_spectrum_name)

 end subroutine set_mcicaWrap

end module soc_set_spectrum

