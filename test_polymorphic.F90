subroutine poly(cp)
  use ISO_C_BINDING
  implicit none
  interface mptr
    function pr41d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real, dimension(:), pointer :: p
    end function pr41d
    function pr42d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real, dimension(:,:), pointer :: p
    end function pr42d
    function pr43d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real, dimension(:,:,:), pointer :: p
    end function pr43d
    function pr44d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:,:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real, dimension(:,:,:,:), pointer :: p
    end function pr44d
    function pr81d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real(kind=8), dimension(:), pointer :: p
    end function pr81d
    function pr82d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real(kind=8), dimension(:,:), pointer :: p
    end function pr82d
    function pr83d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real(kind=8), dimension(:,:,:), pointer :: p
    end function pr83d
    function pr84d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:,:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real(kind=8), dimension(:,:,:,:), pointer :: p
    end function pr84d
    function pi41d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      integer, dimension(:), pointer :: p
    end function pi41d
    function pi42d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      integer, dimension(:,:), pointer :: p
    end function pi42d
    function pi43d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      integer, dimension(:,:,:), pointer :: p
    end function pi43d
    function pi44d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:,:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      integer, dimension(:,:,:,:), pointer :: p
    end function pi44d
    function pi81d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      integer(kind=8), dimension(:), pointer :: p
    end function pi81d
    function pi82d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      integer(kind=8), dimension(:,:), pointer :: p
    end function pi82d
    function pi83d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      integer(kind=8), dimension(:,:,:), pointer :: p
    end function pi83d
    function pi84d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:,:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      integer(kind=8), dimension(:,:,:,:), pointer :: p
    end function pi84d
  end interface
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

  i41 => mptr(cp, i41, [-1, 2])
  print *,'i41',size(i41),lbound(i41,1),ubound(i41,1)
  i42 => mptr(cp, i42, [-1, 2, -2, 1])
  print *,'i42',size(i42),lbound(i42,1),ubound(i42,1),lbound(i42,2),ubound(i42,2)
!   i43 => mptr(cp, i43, [-1, 2, -2, 1, 1, 2])
!   i44 => mptr(cp, i44, [-1, 2, -2, 1, 1, 2, 1, 2])
!   i81 => mptr(cp, i81, [-1, 2])
!   i82 => mptr(cp, i82, [-1, 2, -2, 1])
!   i83 => mptr(cp, i83, [-1, 2, -2, 1, 1, 2])
!   i84 => mptr(cp, i84, [-1, 2, -2, 1, 1, 2, 1, 2])
  r41 => mptr(cp, r41, [-1, 2])
  print *,'r41',size(r41),lbound(r41,1),ubound(r41,1)
  r42 => mptr(cp, r42, [-1, 2, -2, 1])
  print *,'r42',size(r42),lbound(r42,1),ubound(r42,1),lbound(r42,2),ubound(r42,2)
!   r43 => mptr(cp, r43, [-1, 2, -2, 1, 1, 2])
!   r44 => mptr(cp, r44, [-1, 2, -2, 1, 1, 2, 1, 2])
!   r81 => mptr(cp, r81, [-1, 2])
!   r82 => mptr(cp, r82, [-1, 2, -2, 1])
!   r83 => mptr(cp, r83, [-1, 2, -2, 1, 1, 2])
!   r84 => mptr(cp, r84, [-1, 2, -2, 1, 1, 2, 1, 2])
end subroutine poly

function pr41d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real, dimension(:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  real, dimension(:), pointer :: p
  real, dimension(:), pointer :: t

  nullify(p)
!   nullify(po)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2)) => t
!   po(di(1):di(2)) => t
end function pr41d

function pr81d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real(kind=8), dimension(:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  real(kind=8), dimension(:), pointer :: p
  real(kind=8), dimension(:), pointer :: t

  nullify(p)
!   nullify(po)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2)) => t
!   po(di(1):di(2)) => t
end function pr81d

function pi41d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer, dimension(:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  integer, dimension(:), pointer :: p
  integer, dimension(:), pointer :: t

  nullify(p)
!   nullify(po)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2)) => t
!   po(di(1):di(2)) => t
end function pi41d

function pi81d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer(kind=8), dimension(:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  integer(kind=8), dimension(:), pointer :: p
  integer(kind=8), dimension(:), pointer :: t

  nullify(p)
!   nullify(po)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2)) => t
!   po(di(1):di(2)) => t
end function pi81d

function pr42d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real, dimension(:,:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  real, dimension(:,:), pointer :: p
  real, dimension(:), pointer :: t

  nullify(p)
!   nullify(po)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2),di(3):di(4)) => t
!   po(di(1):di(2)) => t
end function pr42d

function pr82d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real(kind=8), dimension(:,:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  real(kind=8), dimension(:,:), pointer :: p
  real(kind=8), dimension(:), pointer :: t

  nullify(p)
!   nullify(po)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2),di(3):di(4)) => t
!   po(di(1):di(2)) => t
end function pr82d

function pi42d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer, dimension(:,:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  integer, dimension(:,:), pointer :: p
  integer, dimension(:), pointer :: t

  nullify(p)
!   nullify(po)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2),di(3):di(4)) => t
!   po(di(1):di(2)) => t
end function pi42d

function pi82d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  integer(kind=8), dimension(:,:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  integer(kind=8), dimension(:,:), pointer :: p
  integer(kind=8), dimension(:), pointer :: t

  nullify(p)
!   nullify(po)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2),di(3):di(4)) => t
!   po(di(1):di(2)) => t
end function pi82d

program test
  use ISO_C_BINDING
  implicit none
  integer, dimension(1000000), target :: dummy
  type(C_PTR) :: cp
  cp = c_loc(dummy(1))
  call poly(cp)
end program