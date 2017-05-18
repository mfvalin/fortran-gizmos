interface
  function strnlen(str,mlen) result(slen) bind(C,name='strnlen')
    import :: C_CHAR, C_SIZE_T
    character(C_CHAR), dimension(*), intent(IN) :: str
    integer(C_SIZE_T), intent(IN), value :: mlen
    integer(C_SIZE_T) :: slen
  end function strnlen
end interface

