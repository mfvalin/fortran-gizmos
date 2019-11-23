  interface shape_c_pointer
    subroutine pr41d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pr41d
    subroutine pr42d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pr42d
    subroutine pr43d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pr43d
    subroutine pr44d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pr44d
    subroutine pr81d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pr81d
    subroutine pr82d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pr82d
    subroutine pr83d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pr83d
    subroutine pr84d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pr84d
    subroutine pi41d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pi41d
    subroutine pi42d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pi42d
    subroutine pi43d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pi43d
    subroutine pi44d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pi44d
    subroutine pi81d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pi81d
    subroutine pi82d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pi82d
    subroutine pi83d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pi83d
    subroutine pi84d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine pi84d
  end interface
