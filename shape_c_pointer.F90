subroutine poly(cp)
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
end subroutine poly

subroutine pr41d(pi,p,di)
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
end subroutine pr41d

subroutine pr81d(pi,p,di)
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
end subroutine pr81d

subroutine pi41d(pi,p,di)
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
end subroutine pi41d

subroutine pi81d(pi,p,di)
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
end subroutine pi81d

subroutine pr42d(pi,p,di)
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
end subroutine pr42d

subroutine pr82d(pi,p,di)
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
end subroutine pr82d

subroutine pi42d(pi,p,di)
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
end subroutine pi42d

subroutine pi82d(pi,p,di)
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
end subroutine pi82d

subroutine pr43d(pi,p,di)
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
end subroutine pr43d

subroutine pr83d(pi,p,di)
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
end subroutine pr83d

subroutine pi43d(pi,p,di)
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
end subroutine pi43d

subroutine pi83d(pi,p,di)
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
end subroutine pi83d

subroutine pr44d(pi,p,di)
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
end subroutine pr44d

subroutine pr84d(pi,p,di)
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
end subroutine pr84d

subroutine pi44d(pi,p,di)
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
end subroutine pi44d

subroutine pi84d(pi,p,di)
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
end subroutine pi84d

program test
  use ISO_C_BINDING
  implicit none
  integer, dimension(1000000), target :: dummy
  type(C_PTR) :: cp
  cp = c_loc(dummy(1))
  call poly(cp)
end program