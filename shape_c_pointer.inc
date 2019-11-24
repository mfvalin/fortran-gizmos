  interface shape_c_pointer
    subroutine shape_c_pointer_r41d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_r41d
    subroutine shape_c_pointer_r42d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_r42d
    subroutine shape_c_pointer_r43d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_r43d
    subroutine shape_c_pointer_r44d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_r44d
    subroutine shape_c_pointer_r81d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_r81d
    subroutine shape_c_pointer_r82d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_r82d
    subroutine shape_c_pointer_r83d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_r83d
    subroutine shape_c_pointer_r84d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real(kind=8), dimension(:,:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_r84d
    subroutine shape_c_pointer_i41d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_i41d
    subroutine shape_c_pointer_i42d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_i42d
    subroutine shape_c_pointer_i43d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_i43d
    subroutine shape_c_pointer_i44d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer, dimension(:,:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_i44d
    subroutine shape_c_pointer_i81d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_i81d
    subroutine shape_c_pointer_i82d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_i82d
    subroutine shape_c_pointer_i83d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_i83d
    subroutine shape_c_pointer_i84d(pi,p,di)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      integer(kind=8), dimension(:,:,:,:), intent(OUT), pointer :: p
      integer, dimension(:), intent(IN) :: di
    end subroutine shape_c_pointer_i84d
  end interface
