interface shm_allocate     // generic interface to allocator into shared memory

  subroutine shm_allocate_i4_1d(p, mini, maxi)
    integer(kind=4), dimension(:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi
  end subroutine shm_allocate_i4_1d
  subroutine shm_allocate_r4_1d(p, mini, maxi)
    real(kind=4), dimension(:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi
  end subroutine shm_allocate_r4_1d
  subroutine shm_allocate_r8_1d(p, mini, maxi)
    real(kind=8), dimension(:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi
  end subroutine shm_allocate_r8_1d

  subroutine shm_allocate_i4_2d(p, mini, maxi, minj, maxj)
    integer(kind=4), dimension(:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj
  end subroutine shm_allocate_i4_2d
  subroutine shm_allocate_r4_2d(p, mini, maxi, minj, maxj)
    real(kind=4), dimension(:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj
  end subroutine shm_allocate_r4_2d
  subroutine shm_allocate_r8_2d(p, mini, maxi, minj, maxj)
    real(kind=8), dimension(:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj
  end subroutine shm_allocate_r8_2d

  subroutine shm_allocate_i4_3d(p, mini, maxi, minj, maxj, mink, maxk)
    integer(kind=4), dimension(:,:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk
  end subroutine shm_allocate_i4_3d
  subroutine shm_allocate_r4_3d(p, mini, maxi, minj, maxj, mink, maxk)
    real(kind=4), dimension(:,:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk
  end subroutine shm_allocate_r4_3d
  subroutine shm_allocate_r8_3d(p, mini, maxi, minj, maxj, mink, maxk)
    real(kind=8), dimension(:,:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk
  end subroutine shm_allocate_r8_3d

  subroutine shm_allocate_i4_4d(p, mini, maxi, minj, maxj, mink, maxk, minl, maxl)
    integer(kind=4), dimension(:,:,:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk, minl, maxl
  end subroutine shm_allocate_i4_4d
  subroutine shm_allocate_r4_4d(p, mini, maxi, minj, maxj, mink, maxk, minl, maxl)
    real(kind=4), dimension(:,:,:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk, minl, maxl
  end subroutine shm_allocate_r4_4d
  subroutine shm_allocate_r8_4d(p, mini, maxi, minj, maxj, mink, maxk, minl, maxl)
    real(kind=8), dimension(:,:,:,:), intent(OUT) :: p
    integer, intent(IN) :: mini, maxi, minj, maxj, mink, maxk, minl, maxl
  end subroutine shm_allocate_r8_4d

end interface
