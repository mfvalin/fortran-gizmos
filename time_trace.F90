! Hopefully useful routines for FORTRAN
! Copyright (C) 2019  Division de Recherche en Prevision Numerique
!                     Environnement Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
module time_trace_mod
  use ISO_C_BINDING
  implicit none

  type, bind(C) :: timeval
    integer(C_LONG_LONG) :: sec
    integer(C_LONG_LONG) :: usec
  end type

  type :: time_context
    type(C_PTR) :: t
  end type

  interface
    function gettime(tv,tz) result(time) BIND(C,name='gettimeofday')
      use iso_c_binding
      import :: timeval
      implicit none
      type(timeval), intent(OUT)     :: tv
      type(C_PTR), value, intent(IN) :: tz
      integer(C_INT) :: time
    end function gettime
  end interface

  integer, parameter :: MAX_TIMES = 1024         ! number of entries in times array
  type :: bead
    type(bead), pointer :: next                  ! pointer to next "bead"
    integer             :: nbent                 ! number of entries used in t
    integer             :: mxent                 ! max dimension of t
    integer, dimension(MAX_TIMES) :: t           ! timings
  end type                                                 


  type :: trace_table
    type(bead), pointer :: first                 ! pointer to first "bead"
    type(bead), pointer :: last                  ! pointer to last (current) "bead"
    logical             :: initialized           ! init flag
    integer             :: step                  ! uninitialized step number
    integer(kind=8)     :: offset                ! time offset (first time, substracted from all subsequent ones)
  end type

  contains

  function what_time_is_it() result (t)
    use ISO_C_BINDING
    implicit none
    INTEGER(C_LONG_LONG) t

    type(timeval) :: tv
    integer(C_INT) :: code! print *,'new_time_step',code,thi,tlo


    code = gettime(tv,C_NULL_PTR)
    t = tv%sec
    t = t * 1000000 + tv%usec
    return
  end function what_time_is_it

  subroutine create_new_bead(tt)                 ! allocate a new "bead" and link it properly
    use ISO_C_BINDING
    implicit none
    type(trace_table), intent(IN), pointer :: tt
  
    type(bead), pointer :: temp

    allocate(temp)
    if( tt%initialized ) then
      tt%last%next => temp                       ! link as next "bead" for last "bead"
    else
      tt%first => temp                           ! special case for first "bead"
      tt%initialized = .true.                    ! set initialized flag
    endif
    temp%next  => NULL()                         ! no next "bead" as this will be the last "bead"
    temp%nbent = 0                               ! zero entries so far
    temp%mxent = MAX_TIMES                       ! max timing entries
!     temp%t(1)  = -1                              ! void timing table
    tt%last => temp                              ! point last to this "bead"
    
  end subroutine create_new_bead

  subroutine trace_insert(tt, val)
    use ISO_C_BINDING
    implicit none
    type(trace_table), intent(IN), pointer :: tt
    integer, intent(IN) :: val
  
    if(tt%last%nbent == tt%last%mxent) call create_new_bead(tt)
    tt%last%nbent = tt%last%nbent + 1
    tt%last%t(tt%last%nbent) = val
    
  end subroutine trace_insert

  subroutine new_time_step(tt, step)             ! insert a new time step into table
    use ISO_C_BINDING
    implicit none
    type(trace_table), intent(IN), pointer :: tt
    integer, intent(IN) :: step
  
    integer(kind=8) :: time, mask
    integer :: thi, tlo, code
  
    time = what_time_is_it()                     ! current time of day in microseconds
    thi  = ishft(time, -32)
    mask = -1
    mask = not(ishft(mask,32))
    tlo  = iand(time, mask)

    code = ishft(step,3)
    call trace_insert(tt,code)
    call trace_insert(tt,thi)
    call trace_insert(tt,tlo)
  end subroutine

  subroutine new_time_tag(tt, tag, t1, t2)       ! insert tag and 1 or 2 time deltas
    use ISO_C_BINDING
    implicit none
    type(trace_table), intent(IN), pointer :: tt
    integer, intent(IN) :: tag
    integer(kind=8), intent(IN) :: t1, t2
  
    integer :: tm1, tm2, code
  
    tm1  = t1 - tt%offset
    if(t2 .ne. 0) then
      tm2  = t2 - tt%offset
      code = ishft(tag,3) + 2                     ! tag followed by 2 32 bit offsets
    else
      code = ishft(tag,3) + 1                     ! tag followed by 1 32 bit offsets
    endif

    call trace_insert(tt,code)
    call trace_insert(tt,tm1)
    if(t2 .ne. 0) call trace_insert(tt,tm2)
    
  end subroutine

end module
!
! user callable subroutines
!
subroutine time_trace_init(t)                      ! create and initialize a new time trace context
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  type(time_context), intent(OUT) :: t

  type(trace_table), pointer :: tt

  allocate(tt)
  tt%initialized = .false.
  tt%step        = -999999
  tt%offset = what_time_is_it()                    ! current time of day in microseconds
  tt%first       => NULL()
  tt%last       => NULL()
  call create_new_bead(tt)
  t%t = C_LOC(tt)

  return
end subroutine time_trace_init

subroutine time_trace_barr(t, tag, barrier, comm, barrier_code, times)   ! insert a new time trace entry (2 entries if barrier is true)
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  external :: barrier_code
  type(time_context), intent(IN) :: t              ! opaque time context pointer
  integer, intent(IN) :: tag                       ! tag number for this timing point (MUST be >0 and <32K-1)
  integer, intent(IN) :: comm                      ! MPI communicator (only used if barrier flag is true)
  logical, intent(IN) :: barrier                   ! if true, call MPI_barrier with timing points before and after
  integer(kind=8), dimension(2), intent(OUT), optional :: times  
                                                   ! times in microseconds returned to user (both values identical if no barrier)
                                                   ! both entries will have the same tag

  integer :: ierr
  type(trace_table), pointer :: tt
  integer(kind=8) :: tt1, tt2

  call C_F_POINTER(t%t, tt)
  tt1 = what_time_is_it() 
  if(barrier) then
    call barrier_code(comm, ierr)                  ! barrier call if needed
    tt2 = what_time_is_it()                        ! time entry after barrier (used to measure imbalance)
  else
    tt2 = 0                                        ! no barrier, set to zero
  endif
  call new_time_tag(tt, tag, tt1, tt2)   ! insert into timing table
  if( present(times) ) then
    times(1) = tt1
    times(2) = tt2
  endif

  return
end subroutine time_trace_barr

subroutine time_trace(t, tag, times)   ! insert a new time trace entry 
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  type(time_context), intent(IN) :: t              ! opaque time context pointer
  integer, intent(IN) :: tag                       ! tag number for this timing point (MUST be >0 and <32K-1)
  integer(kind=8), dimension(2), intent(OUT), optional :: times   ! time in microseconds returned to user

  integer :: ierr
  type(trace_table), pointer :: tt

  call C_F_POINTER(t%t, tt)
  if( present(times) ) then
    times(1) = what_time_is_it()                     ! make time entry
    times(2) = 0
  endif
  call new_time_tag(tt, tag, times(1), times(2))   ! insert into timing table

  return
end subroutine time_trace

subroutine time_trace_step(t, n)   ! set step value for subsequent calls to time_trace
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  integer, intent(IN) :: n
  type(time_context), intent(IN) :: t              ! opaque time context pointer

  integer(kind=8) :: dummy
  type(trace_table), pointer :: tt

  call C_F_POINTER(t%t, tt)
  call new_time_step(tt, n)
  return
end subroutine time_trace_step

subroutine time_trace_dump(t, filename, ordinal)   ! dump timings int file filename_nnnnnn (nnnnnn from ordinal)
  use ISO_C_BINDING
  use time_trace_mod
  implicit none
  type(time_context), intent(IN) :: t              ! opaque time context pointer
  character(len=*), intent(IN) :: filename
  integer, intent(IN) :: ordinal        ! normally MPI rank

  character(len=6) :: extension
  integer :: iun
  type(bead), pointer :: current, next
  integer :: i, j, tag, nval, cstep
  integer, dimension(2) :: tm
  type(trace_table), pointer :: tt
  logical :: finished
  character(len=5) :: str
  integer(kind=8) :: tim8

  call C_F_POINTER(t%t, tt)
  if( .not. associated(tt%first) ) return          ! nothing in tables

  iun = 200
  write(extension,'(I6.6)') ordinal                                ! convert ordinal to 6 character string
  open(iun,file=trim(filename)//'_'//extension//'.txt',form='FORMATTED')   ! build filename and open it in formatted mode

  cstep = -999999
  current => tt%first
  i = 1
  finished = .false.
! print *,'slots used =',current%nbent
  do while(.not. finished)
    tag = 0
    tm  = 0
    nval = 0
    str = ""
    do j = 0, 2    ! up to 3 values
      if(i > current%nbent) then
        current => current%next
        finished = .not. associated(current)
        if(finished) return 
        i = 1
      endif
      if(j == 0) then       ! tag or step
        tag = current%t(i)
        nval = iand(tag, 3)
        if(nval == 0) nval = 2
        if(iand(tag,3) == 0) then 
          str = 'step'
          cstep = ishft(tag,-3)
          tag = -1
        else
          str='tag '
          tag = ishft(tag,-3)
        endif
      else
        if(j > nval) exit
        tm(j) = current%t(i)
      endif
      i = i + 1    ! next value
    enddo
    if(tag == -1) then
      tim8 = tm(1)
      tim8 = ishft(tim8,32)
      tim8 = tim8 + tm(2)
      write(iun,'(I10,",",I10,",",I18,",",I2)')cstep,tag,tim8,0
    else
      write(iun,'(3(I10,","),I10)')cstep,tag,tm
    endif
! write(6,'(A,4(Z8.8,","))')str,cstep,tag,tm
  enddo
  
  close(iun)

  return
end subroutine time_trace_dump

#if defined SELF_TEST
program test_trace
  use ISO_C_BINDING
  implicit none
  include 'time_trace.inc'
#if ! defined(NO_MPI)
  include 'mpif.h'
#else
  integer, parameter :: MPI_COMM_WORLD = 0
  integer, parameter :: MPI_COMM_NULL = -1
#endif
  integer :: ierr, i, tag, rank
  integer(kind=8), dimension(2) :: times
  type(time_context) :: t
  external :: MPI_barrier

  rank = 0
#if ! defined(NO_MPI)
  call MPI_init(ierr)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, ierr)
#endif
  call time_trace_init(t)
  print *,'-----------------------------'
  call time_trace(t, 0, times)
  call time_trace_step(t, 0)
  print *,'============================='
  call time_trace_barr(t, 1, .true., MPI_COMM_WORLD, MPI_barrier, times)
  do i = 1, 3
    call time_trace_step(t, i)
    print *,'+++++++ step =',i
    tag = 10*i+1
    call time_trace_barr(t, tag, .true., MPI_COMM_WORLD, MPI_barrier)
  enddo
  call time_trace_dump(t, 'time_list', rank)
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
