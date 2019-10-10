! useful routines for FORTRAN programming
! Copyright (C) 1975-2019  Meteorological Research Branch
!                          Environnement Canada
! 
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
! 
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this software; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
! 
module shm_allocate_mod
  use ISO_C_BINDING
  interface
    function shm_allocate(nwds) result(p) bind(C,name='ShmAllocate')  // interface to allocator into shared memory
      import :: C_INT, C_PTR
      integer(C_INT), intent(IN), value :: nwds
      type(C_PTR) :: p
    end function shm_allocate
  end interface
end module

subroutine shm_allocate_i4_1d(p, mini, maxi)    // allocate 1D integer array, return pointer
  use shm_allocate_mod
  implicit none
  integer(kind=4), dimension(:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi

  type(C_PTR) :: cp
  integer(kind=4), dimension(:), pointer :: fp
  cp = shm_allocate(maxi-mini+1)
  call C_F_POINTER(cp,fp,[(maxi-mini+1)])
  p(mini:maxi) => fp(:)
end subroutine shm_allocate_i4_1d
subroutine shm_allocate_r4_1d(p, mini, maxi)    // allocate 1D real array, return pointer
  use shm_allocate_mod
  implicit none
  real(kind=4), dimension(:), intent(OUT), pointer :: p
  integer(kind=4), intent(IN) :: mini, maxi

  type(C_PTR) :: cp
  real(kind=4), dimension(:), pointer :: fp
  cp = shm_allocate(maxi-mini+1)
  call C_F_POINTER(cp,fp,[(maxi-mini+1)])
  p(mini:maxi) => fp(:)
end subroutine shm_allocate_r4_1d
subroutine shm_allocate_r8_1d(p, mini, maxi)    // allocate 1D real(kind=8) array, return pointer
  use shm_allocate_mod
  implicit none
  real(kind=8), dimension(:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi

  type(C_PTR) :: cp
  real(kind=8), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*2)
  call C_F_POINTER(cp,fp,[(maxi-mini+1)])
  p(mini:maxi) => fp(:)
end subroutine shm_allocate_r8_1d

subroutine shm_allocate_i4_2d(p, mini, maxi, minj, maxj)    // allocate 2D integer array, return pointer
  use shm_allocate_mod
  implicit none
  integer(kind=4), dimension(:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj

  type(C_PTR) :: cp
  integer(kind=4), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1))
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)])
  p(mini:maxi, minj:maxj) => fp(:)
end subroutine shm_allocate_i4_2d
subroutine shm_allocate_r4_2d(p, mini, maxi, minj, maxj)    // allocate 2D real array, return pointer
  use shm_allocate_mod
  implicit none
  real(kind=4), dimension(:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj

  type(C_PTR) :: cp
  real(kind=4), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1))
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)])
  p(mini:maxi, minj:maxj) => fp(:)
end subroutine shm_allocate_r4_2d
subroutine shm_allocate_r8_2d(p, mini, maxi, minj, maxj)    // allocate 2D real(kind=8) array, return pointer
  use shm_allocate_mod
  implicit none
  real(kind=8), dimension(:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj

  type(C_PTR) :: cp
  real(kind=8), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1)*2)
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)])
  p(mini:maxi, minj:maxj) => fp(:)
end subroutine shm_allocate_r8_2d

subroutine shm_allocate_i4_3d(p, mini, maxi, minj, maxj, mink, maxk)    // allocate 3D integer array, return pointer
  use shm_allocate_mod
  implicit none
  integer(kind=4), dimension(:,:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk

  type(C_PTR) :: cp
  integer(kind=4), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1))
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)])
  p(mini:maxi, minj:maxj, mink:maxk) => fp(:)
end subroutine shm_allocate_i4_3d
subroutine shm_allocate_r4_3d(p, mini, maxi, minj, maxj, mink, maxk)    // allocate 3D real array, return pointer
  use shm_allocate_mod
  implicit none
  real(kind=4), dimension(:,:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk

  type(C_PTR) :: cp
  real(kind=4), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1))
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)])
  p(mini:maxi, minj:maxj, mink:maxk) => fp(:)
end subroutine shm_allocate_r4_3d
subroutine shm_allocate_r8_3d(p, mini, maxi, minj, maxj, mink, maxk)    // allocate 3D real(kind=8) array, return pointer
  use shm_allocate_mod
  implicit none
  real(kind=8), dimension(:,:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk

  type(C_PTR) :: cp
  real(kind=8), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)*2)
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)])
  p(mini:maxi, minj:maxj, mink:maxk) => fp(:)
end subroutine shm_allocate_r8_3d

subroutine shm_allocate_i4_4d(p, mini, maxi, minj, maxj, mink, maxk, minl, maxl)    // allocate 4D integer array, return pointer
  use shm_allocate_mod
  implicit none
  integer(kind=4), dimension(:,:,:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk, minl, maxl

  type(C_PTR) :: cp
  integer, dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)*(maxl-minl+1))
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)*(maxl-minl+1)])
  p(mini:maxi, minj:maxj, mink:maxk, minl:maxl) => fp(:)
end subroutine shm_allocate_i4_4d
subroutine shm_allocate_r4_4d(p, mini, maxi, minj, maxj, mink, maxk, minl, maxl)    // allocate 4D real array, return pointer
  use shm_allocate_mod
  implicit none
  real(kind=4), dimension(:,:,:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk, minl, maxl

  type(C_PTR) :: cp
  real(kind=4), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)*(maxl-minl+1))
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)*(maxl-minl+1)])
  p(mini:maxi, minj:maxj, mink:maxk, minl:maxl) => fp(:)
end subroutine shm_allocate_r4_4d
subroutine shm_allocate_r8_4d(p, mini, maxi, minj, maxj, mink, maxk, minl, maxl)    // allocate 4D real(kind=8) array, return pointer
  use shm_allocate_mod
  implicit none
  real(kind=8), dimension(:,:,:,:), intent(OUT), pointer :: p
  integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk, minl, maxl

  type(C_PTR) :: cp
  real(kind=8), dimension(:), pointer :: fp
  cp = shm_allocate((maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)*(maxl-minl+1)*2)
  call C_F_POINTER(cp,fp,[(maxi-mini+1)*(maxj-minj+1)*(maxk-mink+1)*(maxl-minl+1)])
  p(mini:maxi, minj:maxj, mink:maxk, minl:maxl) => fp(:)
end subroutine shm_allocate_r8_4d

#if defined(SELF_TEST)
function shm_allocate(nwds) result(p) bind(C,name='ShmAllocate')   // dummy allocator for tests
  use ISO_C_BINDING
  implicit none
  integer(C_INT), intent(IN), value :: nwds
  type(C_PTR) :: p

  integer, dimension(:), pointer :: fp
  allocate(fp(nwds))
  p = C_LOC(fp(1))
end function shm_allocate
program selftest
  use ISO_C_BINDING
  implicit none
  include 'shm_allocate.inc'

  integer, dimension(:), pointer      :: pi41
  real, dimension(:), pointer         :: pr41
  real(kind=8), dimension(:), pointer :: pr81
  integer, dimension(:,:), pointer      :: pi42
  real, dimension(:,:), pointer         :: pr42
  real(kind=8), dimension(:,:), pointer :: pr82
  integer, dimension(:,:,:), pointer      :: pi43
  real, dimension(:,:,:), pointer         :: pr43
  real(kind=8), dimension(:,:,:), pointer :: pr83
  integer, dimension(:,:,:,:), pointer      :: pi44
  real, dimension(:,:,:,:), pointer         :: pr44
  real(kind=8), dimension(:,:,:,:), pointer :: pr84

  call shm_allocate(pi41, -4, 5)
  call shm_allocate(pr41, -4, 5)
  call shm_allocate(pr81, -4, 5)
  call shm_allocate(pi42, -4, 5, -4, 5)
  call shm_allocate(pr42, -4, 5, -4, 5)
  call shm_allocate(pr82, -4, 5, -4, 5)
  call shm_allocate(pi43, -4, 5, -4, 5, -4, 5)
  call shm_allocate(pr43, -4, 5, -4, 5, -4, 5)
  call shm_allocate(pr83, -4, 5, -4, 5, -4, 5)
  call shm_allocate(pi44, -4, 5, -4, 5, -4, 5, -4, 5)
  call shm_allocate(pr44, -4, 5, -4, 5, -4, 5, -4, 5)
  call shm_allocate(pr84, -4, 5, -4, 5, -4, 5, -4, 5)
  print *, size(pi41),size(pr41),size(pr81)
  print *, size(pi42),size(pr42),size(pr82)
  print *, size(pi43),size(pr43),size(pr83)
  print *, size(pi44),size(pr44),size(pr84)
end program
#endif

