
type :: time_context  ! wrappper to give a type to a C pointer
  type(C_PTR) :: t
end type

interface

  subroutine time_trace_init(t)                      ! create and initialize a new time trace context
    import :: time_context
    type(time_context), intent(OUT) :: t             ! opaque time context pointer (passed to other routines)
  end subroutine time_trace_init

  subroutine time_trace_dump(t, filename, ordinal)   ! dump timings int file filename_nnnnnn.txt (nnnnnn from ordinal)
    import :: time_context
    implicit none
    type(time_context), intent(IN) :: t              ! opaque time context pointer (from time_trace_init)
    character(len=*), intent(IN) :: filename         ! file name prefix (will be trimmed to remove trailing blanks if any)
    integer, intent(IN) :: ordinal                   ! numbered extension to file name (nnnnnn) (normally MPI rank)
  end subroutine time_trace_dump

  subroutine time_trace_step(t, n)   ! set step value for subsequent calls to time_trace
    import :: time_context
    integer, intent(IN) :: n                         ! time step number
    type(time_context), intent(IN) :: t              ! opaque time context pointer (from time_trace_init)
  end subroutine time_trace_step

  subroutine time_trace_barr(t, tag, barrier, comm, barrier_code, times)   ! insert a new time trace entry (2 entries if barrier is true)
    import :: time_context
    external :: barrier_code
    type(time_context), intent(IN) :: t              ! opaque time context pointer (from time_trace_init)
    integer, intent(IN) :: tag                       ! tag number for this timing point (MUST be >0 and <32K-1)
    integer, intent(IN) :: comm                      ! MPI communicator (only used if barrier flag is true)
    logical, intent(IN) :: barrier                   ! if true, call MPI_barrier with timing points before and after
    integer(kind=8), dimension(2), intent(OUT), optional :: times    ! returned to user (times(2) will be 0 if barrier is .false.)
  end subroutine time_trace_barr

  subroutine time_trace(t, tag, times)               ! insert a new time trace entry (no barrier)
    import :: time_context
    type(time_context), intent(IN) :: t              ! opaque time context pointer (from time_trace_init)
    integer, intent(IN) :: tag                       ! tag number for this timing point (MUST be >0 and <32K-1)
    integer(kind=8), dimension(2), intent(OUT), optional :: times  ! returned to user (times(2) will always be 0)
  end subroutine time_trace

end interface
