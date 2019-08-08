!/* Useful routines for C and FORTRAN programming
! * Copyright (C) 2019  Division de Recherche en Prevision Numerique
! *                     Environnement Canada
! *
! * This is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This software is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! */
subroutine create_msl(msl, tle, phis, ple, ps, n) ! compute msl from tle, phis, ple, ps
  use ISO_C_BINDING
  implicit none
  integer, intent(IN) :: n                     ! number of points
  real, dimension(n), intent(IN)  :: tle       ! temperature at lowest model level
  real, dimension(n), intent(IN)  :: phis      ! surace geopotential
  real, dimension(n), intent(IN)  :: ple       ! pressure at lowest model level
  real, dimension(n), intent(IN)  :: ps        ! surface pressure
  real, dimension(n), intent(OUT) :: msl                   ! mean sea level pressure

  integer :: i
  do i = 1, n
    msl(i) = extrap_msl(tle(i), phis(i), ple(i), ps(i))
  enddo

  return

contains
 
 function extrap_msl(tle, phis, ple, ps) result(msl)
    use ISO_C_BINDING
    implicit none
    real, intent(IN) :: tle       ! temperature at lowest model level
    real, intent(IN) :: phis      ! surace geopotential
    real, intent(IN) :: ple       ! pressure at lowest model level
    real, intent(IN) :: ps        ! surface pressure
    real :: msl                   ! mean sea level pressure

    real, parameter :: RLAPSE = .0065       ! standard lapse rate
    real, parameter :: RGAS   = 287.05      ! gas constant (dry)
    real, parameter :: OVRGAS = 1.0/RGAS
    real, parameter :: GRAV   = 9.80616     ! gravity acceleration at sea level
    real, parameter :: OVGRAV = 1.0/GRAV
    real :: ovphis, ovple, gamma, tsurf, t0, x, ovtsurf

    msl = ps                ! msl = surface pressure if surface is near sea level
    if(phis < .001) return

    ovphis = 1.0 / phis     ! inverse of surface geopotential
    ovple  = 1.0 / ple      ! inverse of pressure at lowest model level
    gamma  = rlapse         ! lapse rate
    tsurf  = tle*( 1.0 + (RLAPSE*RGAS*OVGRAV)*(ps*ovple - 1.0) )  ! surface temperature
    t0     = tsurf + RLAPSE * phis * OVGRAV                       ! mean sea level temperature

    if(tsurf < 255.0) then
      tsurf = .5 * (tsurf + 255.0)
    else if(t0 > 290.5) then
      gamma = max( 0.0 , 290.5 - tsurf ) * GRAV * ovphis
      tsurf = tsurf - max( 0.0 , .5 * (tsurf - 290.5) )
    endif
    ovtsurf = 1.0 / tsurf
    x       = gamma * phis * ovgrav * ovtsurf
    msl     = ps * exp(phis * OVRGAS * ovtsurf * (1 - .5 * x + .333333333 * x * x) )

    return
  end

end
