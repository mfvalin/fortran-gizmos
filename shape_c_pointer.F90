#if defined(NEVER_EVER_TRUE)
!!****P* fortran_gizmos/pointers
!! Synopsis
!! 
!! in Fortran, on can use several kinds of pointers into point to Fortran arrays
!! 
!! Fortran 9x native pointers    
!!      integer, dimension(:,:), pointer     :: ptrf
!!      integer, dimension(ni,nj,nk), target :: z    ! target attribute is mandatory
!!      ptrf(-1:1,-2:2) => z(5:7,3:7,2)              ! ptrf points to a subsection of z
!!      
!! Cray style pointers           
!!      POINTER(ptrc, pointee)
!!      integer, dimension(ni,nj) :: pointee
!!      integer, dimension(ni,nj,nk) :: z
!!      ptrc = loc(z(1,1,3))               ! pointee will access plane 3 of z
!!      
!!      
!! C interoperability pointers  
!!      type(C_PTR) :: ptri
!!      integer, dimension(ni,nj,nk), target :: z    ! target attribute is mandatory
!!      ptri = C_LOC(z(1,1,1))
!!
!! going from one type of pointer to another:
!!
!! C interoperability pointer to Fortran 9x native pointer (2D example)
!!      type(C_PTR)                       :: ptri   ! assumed to point somewhere in memory
!!      integer, dimension(:), pointer    :: temp   ! temporary pointer
!!      integer, dimension(:,:), pointer  :: ptrf   ! desired 2D pointer
!!      call C_F_POINTER(ptri, ptrf, [array_size])  ! make 1D Fortran pointer (lower bound FORCED to 1)
!!      ptrf(mini:maxi,minj:maxj) => ptrf           ! make 2D pointer with arbitrary bounds
!!
!! C interoperability pointer to Cray style pointer
!!      type(C_PTR)                       :: ptri   ! assumed to point somewhere in memory
!!      integer, dimension(ni,nj)         :: pointee
!!      POINTER(ptrc, pointee)
!!      ptrc = transfer(ptri,ptrc)                  ! both are 32 or 64 bit items of the same length
!!
!! Cray style pointer to C interoperability pointer
!!      POINTER(ptrc, pointee)                      ! ptrc assumed to point somewhere in memory
!!      type(C_PTR)  :: ptri
!!      ptri = transfer(ptrc,ptri)                  ! both are 32 or 64 bit items of the same length
!!
!! Cray style pointer to Fortran 9x native pointer
!!      1 - Cray style pointer to C interoperability pointer
!!      2 - C interoperability pointer to Fortran 9x native pointer
!!
!! Fortran 9x native pointer to C interoperability pointer
!!      integer, dimension(:,:), pointer     :: ptrf
!!      type(C_PTR) :: ptri
!!      ptri = C_LOC(ptrf(i,j))
!!
!! Fortran 9x native pointer to C interoperability pointer
!!      integer, dimension(:,:), pointer     :: ptrf
!!      POINTER(ptrc, pointee)
!!      ptrc = LOC(ptrf(i,j))
!!
!! notes:
!! a Cray style pointer is really an integer large enough to contain an adddress
!! the LOC function returns such an integer
!! C_PTR represents a C address and therefore will have the same size as a Cray style pointer
!!****
!!****f* fortran_gizmos/shape_c_pointer
!! Synopsis
!! transform a C pointer (C_PTR type in Fortran from ISO_C_BINDING) into a
!! true Fortran pointer pointing to a 1 to 4 Dimensional array of 
!! 32 or 64 bit Fortran integers or reals
!!
!! call shape_c_pointer(pi,p,di)
!!
!! ARGUMENTS
! pointer to actual memory area
  type(C_PTR), intent(IN)           :: pi
! array of dimension at least 2 * number of dimensions containing the bounds of the dimensions
  integer, dimension(:), intent(IN) :: di    
! p is a Fortran native pointer, INTENT(OUT), 1 to 4 dimensions, 32 or 64 bit real or integer
!! EXAMPLES
  program test
  use ISO_C_BINDING
  implicit none
  integer, dimension(1000000), target :: dummy
  type(C_PTR) :: cp      ! C interoperability pointer
  cp = c_loc(dummy(1))   ! set it to address of array
  call example(cp)
  end program
  subroutine example(cp)
  use ISO_C_BINDING
  implicit none
  include 'shape_c_pointer.inc'
  type(C_PTR), intent(IN) :: cp  ! pointer to actual storage

  integer, dimension(:), pointer       :: i41
  integer(kind=8), dimension(:,:), pointer     :: i82
  real, dimension(:,:,:), pointer   :: r43
  real(kind=8), dimension(:,:,:,:), pointer :: r84

! one dimensional array, the first 2 values of di will be used 
  call shape_c_pointer(cp, i41, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i41, size=',size(i41),' bounds=',lbound(i41,1),ubound(i41,1)
! two dimensional array, the first 4 values of di will be used 
  call shape_c_pointer(cp, i82, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i82, size=',size(i82),' bounds=',lbound(i82,1),ubound(i82,1),&
                                            lbound(i82,2),ubound(i82,2)
! three dimensional array, the first 6 values of di will be used 
  call shape_c_pointer(cp, r43, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r43, size=',size(r43),' bounds=',lbound(r43,1),ubound(r43,1),&
                                            lbound(r43,2),ubound(r43,2),&
                                            lbound(r43,3),ubound(r43,3)
! four dimensional array, the first 8 values of di will be used 
  call shape_c_pointer(cp, r84, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r84, size=',size(r84),' bounds=',lbound(r84,1),ubound(r84,1),&
                                            lbound(r84,2),ubound(r84,2),&
                                            lbound(r84,3),ubound(r84,3),&
                                            lbound(r84,4),ubound(r84,4)
1 format(A,I8,A,8I4)
end subroutine

expected output:
i41, size=       4 bounds=  -1   2
i82, size=      16 bounds=  -1   2  -2   1
r43, size=      64 bounds=  -1   2  -2   1  -3   0
r84, size=     256 bounds=  -1   2  -2   1  -3   0  -4  -1
!!****
#endif
#if defined(SELF_TEST)
program test
  use ISO_C_BINDING
  implicit none
  integer, dimension(1000000), target :: dummy
  type(C_PTR) :: cp
  cp = c_loc(dummy(1))
  call shape_c_pointer_test(cp)
end program

subroutine shape_c_pointer_test(cp)
  use ISO_C_BINDING
  implicit none
  include 'shape_c_pointer.inc'
  type(C_PTR), intent(IN) :: cp
  integer, dimension(:), pointer       :: i41
  integer, dimension(:,:), pointer     :: i42
  integer, dimension(:,:,:), pointer   :: i43
  integer, dimension(:,:,:,:), pointer :: i44
  integer(kind=8), dimension(:), pointer       :: i81
  integer(kind=8), dimension(:,:), pointer     :: i82
  integer(kind=8), dimension(:,:,:), pointer   :: i83
  integer(kind=8), dimension(:,:,:,:), pointer :: i84
  real, dimension(:), pointer       :: r41
  real, dimension(:,:), pointer     :: r42
  real, dimension(:,:,:), pointer   :: r43
  real, dimension(:,:,:,:), pointer :: r44
  real(kind=8), dimension(:), pointer       :: r81
  real(kind=8), dimension(:,:), pointer     :: r82
  real(kind=8), dimension(:,:,:), pointer   :: r83
  real(kind=8), dimension(:,:,:,:), pointer :: r84

  call shape_c_pointer(cp, i41, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i41, size=',size(i41),' bounds=',lbound(i41,1),ubound(i41,1)
  call shape_c_pointer(cp, i42, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i42, size=',size(i42),' bounds=',lbound(i42,1),ubound(i42,1),lbound(i42,2),ubound(i42,2)
  call shape_c_pointer(cp, i43, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i43, size=',size(i43),' bounds=',lbound(i43,1),ubound(i43,1),lbound(i43,2),ubound(i43,2),lbound(i43,3),ubound(i43,3)
  call shape_c_pointer(cp, i44, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i44, size=',size(i44),' bounds=',lbound(i44,1),ubound(i44,1),lbound(i44,2),ubound(i44,2),lbound(i44,3),ubound(i44,3),lbound(i44,4),ubound(i44,4)
  call shape_c_pointer(cp, i81, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i81, size=',size(i81),' bounds=',lbound(i81,1),ubound(i81,1)
  call shape_c_pointer(cp, i82, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i82, size=',size(i82),' bounds=',lbound(i82,1),ubound(i82,1),lbound(i82,2),ubound(i82,2)
  call shape_c_pointer(cp, i83, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i83, size=',size(i83),' bounds=',lbound(i83,1),ubound(i83,1),lbound(i83,2),ubound(i83,2),lbound(i83,3),ubound(i83,3)
  call shape_c_pointer(cp, i84, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'i84, size=',size(i84),' bounds=',lbound(i84,1),ubound(i84,1),lbound(i84,2),ubound(i84,2),lbound(i84,3),ubound(i84,3),lbound(i84,4),ubound(i84,4)
  call shape_c_pointer(cp, r41, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r41, size=',size(r41),' bounds=',lbound(r41,1),ubound(r41,1)
  call shape_c_pointer(cp, r42, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r42, size=',size(r42),' bounds=',lbound(r42,1),ubound(r42,1),lbound(r42,2),ubound(r42,2)
  call shape_c_pointer(cp, r43, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r43, size=',size(r43),' bounds=',lbound(r43,1),ubound(r43,1),lbound(r43,2),ubound(r43,2),lbound(r43,3),ubound(r43,3)
  call shape_c_pointer(cp, r44, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r44, size=',size(r44),' bounds=',lbound(r44,1),ubound(r44,1),lbound(r44,2),ubound(r44,2),lbound(r44,3),ubound(r44,3),lbound(r44,4),ubound(r44,4)
  call shape_c_pointer(cp, r81, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r81, size=',size(r81),' bounds=',lbound(r81,1),ubound(r81,1)
  call shape_c_pointer(cp, r82, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r82, size=',size(r82),' bounds=',lbound(r82,1),ubound(r82,1),lbound(r82,2),ubound(r82,2)
  call shape_c_pointer(cp, r83, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r83, size=',size(r83),' bounds=',lbound(r83,1),ubound(r83,1),lbound(r83,2),ubound(r83,2),lbound(r83,3),ubound(r83,3)
  call shape_c_pointer(cp, r84, [-1, 2, -2, 1, -3, 0 ,-4, -1])
  print 1,'r84, size=',size(r84),' bounds=',lbound(r84,1),ubound(r84,1),lbound(r84,2),ubound(r84,2),lbound(r84,3),ubound(r84,3),lbound(r84,4),ubound(r84,4)
1 format(A,I8,A,8I4)
end subroutine shape_c_pointer_test
#endif
subroutine shape_c_pointer_r41d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real, dimension(:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  real, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)])
  p(di(1):di(2)) => t
end subroutine shape_c_pointer_r41d

subroutine shape_c_pointer_r81d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real(kind=8), dimension(:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  real(kind=8), dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)])
  p(di(1):di(2)) => t
end subroutine shape_c_pointer_r81d

subroutine shape_c_pointer_i41d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer, dimension(:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  integer, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)])
  p(di(1):di(2)) => t
end subroutine shape_c_pointer_i41d

subroutine shape_c_pointer_i81d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer(kind=8), dimension(:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  integer(kind=8), dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)])
  p(di(1):di(2)) => t
end subroutine shape_c_pointer_i81d

subroutine shape_c_pointer_r42d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real, dimension(:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  real, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)])
  p(di(1):di(2),di(3):di(4)) => t
end subroutine shape_c_pointer_r42d

subroutine shape_c_pointer_r82d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real(kind=8), dimension(:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  real(kind=8), dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)])
  p(di(1):di(2),di(3):di(4)) => t
end subroutine shape_c_pointer_r82d

subroutine shape_c_pointer_i42d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer, dimension(:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  integer, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)])
  p(di(1):di(2),di(3):di(4)) => t
end subroutine shape_c_pointer_i42d

subroutine shape_c_pointer_i82d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer(kind=8), dimension(:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  integer(kind=8), dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)])
  p(di(1):di(2),di(3):di(4)) => t
end subroutine shape_c_pointer_i82d

subroutine shape_c_pointer_r43d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real, dimension(:,:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  real, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 6) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)*(di(6)-di(5)+1)])
  p(di(1):di(2),di(3):di(4),di(5):di(6)) => t
end subroutine shape_c_pointer_r43d

subroutine shape_c_pointer_r83d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real(kind=8), dimension(:,:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  real(kind=8), dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 6) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)*(di(6)-di(5)+1)])
  p(di(1):di(2),di(3):di(4),di(5):di(6)) => t
end subroutine shape_c_pointer_r83d

subroutine shape_c_pointer_i43d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer, dimension(:,:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  integer, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 6) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)*(di(6)-di(5)+1)])
  p(di(1):di(2),di(3):di(4),di(5):di(6)) => t
end subroutine shape_c_pointer_i43d

subroutine shape_c_pointer_i83d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer(kind=8), dimension(:,:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  integer(kind=8), dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 6) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)*(di(6)-di(5)+1)])
  p(di(1):di(2),di(3):di(4),di(5):di(6)) => t
end subroutine shape_c_pointer_i83d

subroutine shape_c_pointer_r44d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real, dimension(:,:,:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  real, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 8) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)*(di(6)-di(5)+1)*(di(8)-di(7)+1)])
  p(di(1):di(2),di(3):di(4),di(5):di(6),di(7):di(8)) => t
end subroutine shape_c_pointer_r44d

subroutine shape_c_pointer_r84d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real(kind=8), dimension(:,:,:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  real(kind=8), dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 8) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)*(di(6)-di(5)+1)*(di(8)-di(7)+1)])
  p(di(1):di(2),di(3):di(4),di(5):di(6),di(7):di(8)) => t
end subroutine shape_c_pointer_r84d

subroutine shape_c_pointer_i44d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer, dimension(:,:,:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  integer, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 8) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)*(di(6)-di(5)+1)*(di(8)-di(7)+1)])
  p(di(1):di(2),di(3):di(4),di(5):di(6),di(7):di(8)) => t
end subroutine shape_c_pointer_i44d

subroutine shape_c_pointer_i84d(pi,p,di)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer(kind=8), dimension(:,:,:,:), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di
  integer(kind=8), dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 8) return
  call C_F_POINTER(pi, t, [(di(2)-di(1)+1)*(di(4)-di(3)+1)*(di(6)-di(5)+1)*(di(8)-di(7)+1)])
  p(di(1):di(2),di(3):di(4),di(5):di(6),di(7):di(8)) => t
end subroutine shape_c_pointer_i84d
