! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Wraps socrates cloud generation.


module soc_cloud_gen

USE interface_core
Use, intrinsic :: ISO_C_BINDING, Only: C_INT,C_BOOL,C_DOUBLE

implicit none
character(len=*), parameter, private :: ModuleName = 'SOC_CLOUD_GEN'

contains


subroutine cloud_genwrap(nd_layer, cloud_top, n_layer, nd_profile, il1, il2, &
                     n_subcol, n1, n2, &
                     ipph, ioverlap, rand_seed, &
                     rlc_cf, rlc_cw, sigma_qcw, avg_cf, &
                     c_cloud, c_ratio, zf, xcw, &
                     n_subcol_cld, c_sub) BIND(C,name='cloud_gen')


  implicit none

  ! Input
   Integer (C_INT), intent(in) :: &
    nd_layer,            & ! Size of layer dimension
    cloud_top,           & ! Top cloudy layer
    n_layer,             & ! Number of layers in GCM
    nd_profile,          & ! Dimension size for GCM columns
    il1,                 & ! Start GCM column
    il2,                 & ! End GCM column
    n_subcol,            & ! Number of sub-columns to generate
    n1, n2,              & ! Dimensions of xcw array
    ipph,                & ! Horizontal homogeneity
    ioverlap,            & ! Vertical overlap
    rand_seed(nd_profile)  ! Seed for generating random numbers

   real (C_DOUBLE), intent(in) :: &
    zf(nd_profile, nd_layer), &
  !     Full-level (layer midpoint) pressure (Pa)
    avg_cf(nd_profile, cloud_top:nd_layer), &
  !     Cloud amount for each layer
    c_cloud(nd_profile, cloud_top:nd_layer), &
  !     Convective cloud amount for each layer
    c_ratio(nd_profile, cloud_top:nd_layer), &
  !     Convective condensate ratio
    sigma_qcw(nd_profile, cloud_top:nd_layer), &
  !     Normalized cloud condensate std. dev. (Std. dev. over mean)
    rlc_cf(nd_profile, cloud_top:nd_layer), &
  !      Cloud fraction decorrelation scale (Pa)
    rlc_cw(nd_profile, cloud_top:nd_layer), &
  !     Cloud condensate decorrelation scale (Pa)
    xcw(n1, n2)
  !     Distribution of normalised condensate amount as a function of
  !     cumulative probability (n1) and relative standard deviation (n2)

  ! Output
  Integer(C_INT), intent(out) :: n_subcol_cld(nd_profile)
  !     Number of cloudy sub-columns

  real(C_DOUBLE), intent(inout) :: c_sub(nd_profile, cloud_top:nd_layer, n_subcol)
  !     Sub-grid cloud water content
  
 
  call cloud_gen(nd_layer, cloud_top, n_layer, nd_profile, il1, il2, &
                     n_subcol, n1, n2, &
                     ipph, ioverlap, rand_seed, &
                     rlc_cf, rlc_cw, sigma_qcw, avg_cf, &
                     c_cloud, c_ratio, zf, xcw, &
                     n_subcol_cld, c_sub) 


 end subroutine cloud_genwrap

end module soc_cloud_gen