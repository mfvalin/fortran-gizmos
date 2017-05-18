interface
!
  function libc_strlen(str) result(slen) bind(C,name='strlen')
    import :: C_CHAR, C_SIZE_T
    character(C_CHAR), dimension(*), intent(IN) :: str
    integer(C_SIZE_T) :: slen
  end function libc_strlen
!
  function libc_strnlen(str,mlen) result(slen) bind(C,name='strnlen')
    import :: C_CHAR, C_SIZE_T
    character(C_CHAR), dimension(*), intent(IN) :: str
    integer(C_SIZE_T), intent(IN), value :: mlen
    integer(C_SIZE_T) :: slen
  end function libc_strnlen
!
  function libc_alarm(seconds) result(seconds_since) BIND(C,name='alarm')
    use ISO_C_BINDING
    implicit none
    integer(C_INT), intent(IN), value :: seconds
    integer(C_INT) :: seconds_since
  end function libc_alarm
!
  function libc_gethostid() result(id) BIND(C,name='gethostid')  ! interface to libc gethostid
    import :: C_INT
    integer(C_INT) :: id
  end function libc_gethostid
!
  function libc_malloc(siz) result (p) bind(C,name='malloc')
    import C_SIZE_T, C_PTR
    integer(C_SIZE_T), intent(IN), value :: siz
    type(C_PTR) :: p
  end function libc_malloc
!
  subroutine libc_free(p) bind(C,name='free')
    import C_PTR
    type(C_PTR), intent(IN), value :: p
  end subroutine libc_free
!
  subroutine libc_sleep(duration) bind(C,name='sleep')
    use ISO_C_BINDING
    integer(C_INT), intent(IN), value :: duration
    integer(C_INT) :: status
  end subroutine libc_sleep
!
end interface

