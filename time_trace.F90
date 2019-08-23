module time_trace_mod
  use ISO_C_BINDING
  implicit none

  type, bind(C) :: timeval
    integer(C_LONG_LONG) :: sec
    integer(C_LONG_LONG) :: usec
  end type

  interface
    function gettime(tv,tz) result(code) BIND(C,name='gettimeofday')
      use iso_c_binding
      import :: timeval
      implicit none
      type(timeval), intent(OUT) :: tv
      type(C_PTR), value, intent(IN) :: tz
      integer(C_INT) :: code
    end function gettime
  end interface

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

  function new_time_entry(tag) result(t)               ! insert a new time tag, return time in microseconds
    use ISO_C_BINDING
    implicit none
    integer, intent(IN) :: tag
    integer(kind=8) :: t
  
    integer(kind=8) :: newtime, tag8
    
    newtime = what_time_is_it()                  ! current time of day in microseconds
    t = newtime                                  ! return RAW time in microseconds
    if(offset < 0) offset = newtime              ! initialize offset if not already initialized
    newtime = newtime - offset                   ! substract initial offset
    tag8 = tag
    if(tag == 32767) newtime = step
    newtime = ior(newtime , ishft(tag8,48) )     ! move tag to upper part
    if(last%nbent == last%mxent) call create_new_bead
    last%nbent = last%nbent + 1
    last%t(last%nbent) = newtime
print *,'tag =',tag
    return
  end function new_time_entry

  function what_time_is_it() result (t)
    use ISO_C_BINDING
    implicit none
    INTEGER(C_LONG_LONG) t

    type(timeval) :: tv
    integer(C_INT) :: code

    code = gettime(tv,C_NULL_PTR)
    t = tv%sec
    t = t * 1000000 + tv%usec
    return
  end function what_time_is_it
end module
!
! user callable subroutines
!
subroutine time_trace_init                         ! initialize package
  use ISO_C_BINDING
  use time_trace_mod
  implicit none

  if(initialized) return
  call create_new_bead
  offset = what_time_is_it()                       ! initial time of day in microseconds

  return
end subroutine time_trace_init

subroutine time_trace(tag, barrier, comm, times, barrier_code)   ! insert a new time trace entry (2 entries if barrier is true)
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  external :: barrier_code
  integer, intent(IN) :: tag                       ! tag number for this timing point (MUST be >0 and <32K-1)
  integer, intent(IN) :: comm                      ! MPI communicator (only used if barrier flag is true)
  logical, intent(IN) :: barrier                   ! if true, call MPI_barrier with timing points before and after
  integer(kind=8), dimension(2), intent(OUT) :: times  
                                                   ! times in microseconds returned to user (both values identical if no barrier)
                                                   ! both entries will have the same tag

  integer :: ierr

  if(.not. initialized) call time_trace_init

  times(1) = new_time_entry(tag)                         ! make time entry
  if(barrier) then
    call barrier_code(comm, ierr)                   ! barrier call if needed
    times(2) = new_time_entry(tag)                       ! time entry after barrier (used to measure imbalance)
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
  dummy = new_time_entry(32*1024 - 1)    ! special tag 32765 indication change of step
  return
end subroutine time_trace_step

subroutine time_trace_dump(filename, ordinal)  ! dump timings int file filename_nnnnnn (nnnnnn from ordinal)
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  character(len=*), intent(IN) :: filename
  integer, intent(IN) :: ordinal        ! normally MPI rank

  character(len=6) :: extension
  integer :: iun
  type(bead), pointer :: current, next
  integer :: i, cstep, tag
  integer(kind=8) :: time

  if(.not. initialized) return  ! package not initialized, nothing to do

  iun = 200
  write(extension,'(I6.6)') ordinal                                ! convert ordinal to 6 character string
  open(iun,file=trim(filename)//'_'//extension//'.txt',form='FORMATTED')   ! build filename and open it in formatted mode

  cstep = -999999
  current => first
  do while(associated(current))
    do i = 1, current%nbent
      if( iand(32767,ishft(current%t(i),-48)) == 32767) then   ! step marker
        cstep = iand(current%t(i),32767)
      else
        tag = iand( ishft(current%t(i),-48) , 32767)
        time = iand(current%t(i),not(ishft(32767_8,48)))
        write(iun,'(I8,",",I8,",",I16)') cstep, tag, time
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
#if ! defined(NO_MPI)
  include 'mpif.h'
#else
  integer, parameter :: MPI_COMM_WORLD = 0
  integer, parameter :: MPI_COMM_NULL = -1
#endif
  integer :: ierr, i, tag, rank
  integer(kind=8), dimension(2) :: times
  external :: MPI_barrier

  rank = 0
#if ! defined(NO_MPI)
  call MPI_init(ierr)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, ierr)
#endif
  call time_trace_init
  call time_trace(0, .false., MPI_COMM_NULL, times, MPI_barrier)
  call time_trace_step(0)
  print *,'============================='
  call time_trace(1, .true., MPI_COMM_WORLD, times, MPI_barrier)
  do i = 1, 3
    call time_trace_step(i)
    print *,'+++++++ step =',i
    tag = 10*i+1
    call time_trace(tag, .true., MPI_COMM_WORLD, times, MPI_barrier)
  enddo
  call time_trace_dump('time_list', rank)
  print *,'============================='
#if ! defined(NO_MPI)
  call MPI_finalize(ierr)
#endif

  stop
end program
#if defined(NO_MPI)
  subroutine MPI_barrier(dummy1, dummy2)
    integer, intent(IN) :: dummy1, dummy2
  end subroutine MPI_barrier
#endif
#endif
