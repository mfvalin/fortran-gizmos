program fortran_main
! fake fortran main for C program that calls fortran routines that
! might require fortran runtime library initialization
!
! cc -Dmain=My_C_Main mydemo.c
! gfortran  Fortran_to_c_main.F90 mydemo.o
! ./a.out arg1 arg2 arg3
!
!==================================================================================
! simple example
!
! cat mydemo.c
!int main(int argc, char**argv)
!{
!  int i;
!  for(i=0;i<argc;i++) printf("arg %d = '%s'\n",i,argv[i]);
!  return(0);
!}
!
  use ISO_C_BINDING
  implicit none
  integer(C_INT) :: nargs
  integer :: i, length, status
  character(len=4096) :: argument
  character(len=1), dimension(:), pointer :: arg1
  type(C_PTR), dimension(:), pointer :: argv
  type(C_PTR) :: argtab
!
  interface   ! call real C main
    function c_main(nargs,argv) result(status) BIND(C,name='My_C_Main')
    import
    implicit none
    integer(C_INT), intent(IN), value :: nargs      ! int argc
    type(C_PTR), intent(IN), value :: argv   ! char **argv
    integer(C_INT) :: status
    end function c_main
  end interface
!
  nargs = command_argument_count()
  allocate(argv(0:nargs+1))       ! allocate table of C pointers
  argv = C_NULL_PTR               ! initialize to NULL pointers
  do i=0,nargs
    call get_command_argument(i,argument,length,status)       ! get argument i
    allocate(arg1(length+1))                                  ! allocate byte array with room for null terminator
    arg1 = transfer(trim(argument)//achar(0),arg1,length+1)   ! copy null terminated string into C compatible char array
    argv(i) = C_LOC(arg1(1))                                  ! put pointer into slot i of array of argument pointers
  enddo
  argtab = C_LOC(argv(0))          ! pointer to array of pointers to individual arguments
  status = c_main(nargs+1,argtab)    ! call our C main, ginving it what it expects (int argc, char**argv)
  stop
end
