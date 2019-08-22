module time_trace_mod
  use ISO_C_BINDING
  implicit none

  real(kind=8), external :: MPI_wtime
  integer, parameter :: MAX_TIMES = 1024         ! number of entries in times array
  type :: bead
    type(bead), pointer :: next                  ! pointer to next "bead"
    integer :: nbent                             ! number of entries used in t
    integer :: mxent                             ! max dimension of t
    integer(kind=8), dimension(MAX_TIMES) :: t   ! timings
  end type

  type(bead), pointer, save :: first => NULL()   ! pointer to first "bead"
  type(bead), pointer, save :: last => NULL()    ! pointer to last (current) "bead"
  logical, save :: initialized = .false.         ! init flag
  integer, save :: step = -999999                ! uninitialized step number
  integer(kind=8) :: offset = -1                 ! time offset (first time, substracted from all subsequent ones)

  contains

  subroutine create_new_bead                     ! allocate a new "bead" and link it properly
    use ISO_C_BINDING
    implicit none
  
    type(bead), pointer :: temp

    allocate(temp)
    if( initialized ) then
      last%next => temp                          ! link as next "bead" for last "bead"
    else
      first => temp                              ! special case for first "bead"
      initialized = .true.                       ! set initialized flag
    endif
    temp%next => NULL()                          ! no next "bead" as this will be the last "bead"
    temp%mxent = MAX_TIMES                       ! max timing entries
    temp%nbent = 0                               ! zero entries so far
    temp%t = -1                                  ! void timing table
    last => temp                                 ! point last to this "bead"
    
  end subroutine create_new_bead

  function new_time(tag) result(t)               ! insert a new time tag, return time in microseconds
    use ISO_C_BINDING
    implicit none
    integer, intent(IN) :: tag
    integer(kind=8) :: t
  
    integer(kind=8) :: newtime
    
    newtime = MPI_wtime() * 1000000.0_8          ! convert current time to microseconds
    t = newtime                                  ! return RAW time in microseconds
    if(offset < 0) offset = newtime              ! initialize offset if not already initialized
    newtime = newtime - offset                   ! substract initial offset
    newtime = ior(newtime , ishft(tag,48) )      ! move tag to upper part
    if(last%nbent == last%mxent) call create_new_bead
    last%nbent = last%nbent + 1
    last%t(last%nbent) = newtime
    return
  end function new_time
end module

subroutine time_trace_init                       ! initialize package
  use ISO_C_BINDING
  use time_trace_mod
  implicit none

  if(initialized) return
  call create_new_bead
  offset = MPI_wtime() * 1000000.0_8             ! convert initial time to microseconds
  
  return
end subroutine time_trace_init

subroutine time_trace(tag, barrier, comm, times)   ! insert a new time trace entry (2 entries if barrier is true)
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  integer, intent(IN) :: tag                       ! tag number for this timing point (MUST be >0 and <32K-1)
  integer, intent(IN) :: comm                      ! MPI communicator (only used if barrier flag is true)
  logical, intent(IN) :: barrier                   ! if true, call MPI_barrier with timing points before and after
  integer(kind=8), dimension(2), intent(OUT) :: times  
                                                   ! times in microseconds returned to user (both values identical if no barrier)
                                                   ! both entries will have the same tag

  integer :: ierr

  if(.not. initialized) call time_trace_init

  times(1) = new_time(tag)                         ! make time entry
  if(barrier) then
    call MPI_barrier(comm, ierr)                   ! barrier call if needed
    times(2) = new_time(tag)                       ! time entry after barrier (used to measure imbalance)
  else
    times(2) = times(1)                            ! no barrier, same as times(1)
  endif

  return
end subroutine time_trace

subroutine time_trace_step(n)   ! set step value for subsequent calls to time_trace
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  integer, intent(IN) :: n
  integer(kind=8) :: dummy

  if(.not. initialized) call time_trace_init

  step = n
  dummy = new_time(32*1024 - 1)    ! special tag 32765 indication change of step
  return
end subroutine time_trace_step

subroutine time_trace_dump(filename, ordinal)  ! dump timings int file filename_nnnnnn (nnnnnn from ordinal)
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  character(len=*), intent(IN) :: filename
  integer, intent(IN) :: ordinal        ! normally mpi rank

  character(len=6) :: extension
  integer :: iun
  type(bead), pointer :: current, next
  integer :: i, cstep, tag
  integer(kind=8) :: time

  if(.not. initialized) return  ! package not initialized, nothing to do

  iun = 200
  write(extension,'(I6.6)') ordinal                                ! convert ordinal to 6 character string
  open(iun,file=trim(filename)//'_'//extension,form='FORMATTED')   ! build filename and open it in formatted mode

  cstep = -999999
  current => first
  do while(associated(current))
    do i = 1, current%nbent
      if(ishft(current%t(i),-48) == -1) then   ! step marker
        cstep = iand(current%t(i),32767)
      else
        tag = iand( ishft(current%t(i),-48) , 32767)
        time = iand(current%t(i),not(ishft(32767,48)))
        write(iun,'(I8,I8,I16)') cstep, tag, time
      endif
    enddo
    current => current%next
  enddo
  
  close(iun)

  return
end subroutine time_trace_dump

#if defined SELF_TEST
program test_trace
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'
  integer :: ierr

  call mpi_init(ierr)
  print *,'============================='
  call mpi_finalize(ierr)

  stop
end program
#endif
