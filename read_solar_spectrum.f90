! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!+ Subroutine to read a solar spectrum.
!
SUBROUTINE read_solar_spectrum(SolarSpec, ierr) 

! Description:
!   The solar spectrum is read as a file of irradiances against
!   wavelengths.

  USE realtype_rd, ONLY: RealK
  USE def_std_io_icf, ONLY: iu_err
  USE def_solarspec, ONLY: StrSolarSpec
  USE rad_pcf, ONLY: i_normal, i_err_fatal

  IMPLICIT NONE


! Dummy arguments.
  INTEGER, Intent(INOUT) :: ierr
!   Error flag
  TYPE (StrSolarSpec), Intent(OUT) :: SolarSpec
!   Solar spectrum

! Local variables.
  CHARACTER (LEN=80) :: line
!   Line of input data
  INTEGER :: iu_solar
!   Unit number for the solar spectrum
  INTEGER :: ios
!   I/O error flag
  INTEGER :: i
!   Loop variable
  LOGICAL :: l_count
!   Flag for counting points
  REAL (RealK) :: scale_wv, scale_irr
!   Scaling for wavelength and irradiance to correct units


! Obtain the file containing the solar spectrum.
  CALL get_free_unit(ierr, iu_solar)
  CALL open_file_in(ierr, iu_solar, & 
    'Enter the name of the file containing the solar irradiance data.')
  IF (ierr /= i_normal) RETURN
!
! Read first to find the number of points in the spectrum.
  SolarSpec%n_points = 0
  l_count=.FALSE.
  DO
    READ(iu_solar, '(A)', IOSTAT=ios) line
    IF (ios /= 0) THEN
      EXIT
    ELSE IF (line(1:11) == '*BEGIN_DATA') THEN
      l_count=.TRUE.
    ELSE IF (line(1:4) == '*END') THEN
      l_count=.FALSE.
    ELSE IF (l_count) THEN
      SolarSpec%n_points=SolarSpec%n_points+1
    ENDIF
  ENDDO
!
  ALLOCATE(SolarSpec%wavelength(SolarSpec%n_points))
  ALLOCATE(SolarSpec%irrad(SolarSpec%n_points))
!
! Read in the file.
  scale_wv=1.0_RealK
  scale_irr=1.0_RealK
  REWIND(iu_solar)
  DO
    READ(iu_solar, '(A)', IOSTAT=ios) line
    IF (line(1:11) == '*BEGIN_DATA') THEN
      DO i = 1, SolarSpec%n_points
        READ(iu_solar, *, IOSTAT=ios) &
          SolarSpec%wavelength(i), &
          SolarSpec%irrad(i)
        IF (ios /= 0) THEN
          WRITE(iu_err, '(/A)') '*** Error: Corrupt solar spectrum.'
          ierr=i_err_fatal
          RETURN
        ENDIF
      ENDDO
      EXIT
    ENDIF
    IF (line(1:11) == '*SCALE_DATA') THEN
      READ(iu_solar, *, IOSTAT=ios) scale_wv, scale_irr
    ENDIF
    IF (line(1:7) == '*RADIUS') THEN
      READ(iu_solar, *, IOSTAT=ios) SolarSpec%radius
    END IF
    IF (line(1:12) == '*TEMPERATURE') THEN
      READ(iu_solar, *, IOSTAT=ios) SolarSpec%t_effective
    END IF
  ENDDO

! Check that reading was carried out: failure to read the data
! will cause n_solar_points to be 0.
  IF (SolarSpec%n_points == 0) THEN
    WRITE(iu_err, '(/a)') &
      '*** Error: No data were read. Check format of file of irradiances.'
    ierr=i_err_fatal
    RETURN
  ENDIF

! Scale the values to the correct units:
  SolarSpec%wavelength = SolarSpec%wavelength * scale_wv
  SolarSpec%irrad      = SolarSpec%irrad      * scale_irr

END SUBROUTINE read_solar_spectrum
