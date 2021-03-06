integer function oil_spill_fill(datafield,ni,nj,mask)
!
! fill array datafield that has holes (missing values) using the valid values
! array mask tells whether data points are valid or missing
! mask(i,J)==0 means datafield(i,j) is missing (hole)
!
! Missing values are filled using adjacent data points.
! In order to fill missing value datafield(i,j), it is necessary 
! to have at least TWO valid (non missing) adjacent datapoints.
! The number of adjacent point is usually 8, except if point (i,j)
! lies on the boundary (first/last row or first/last column)
! This means that multiple filling passes will have to be performed
! in order to fill all missing values.
! Values filled during pass N can be used to compute fill values
! during subsequent passes (but not during pass N)
!
! The function returns the number of points it has not been able to fill.
! If a pass was unable to fill any point while there were still holes in the 
! array, the function quits before entering an endless loop.
!
!         (i-1,j+1)        (i  ,J+1)      (i+1,j+1)
!
!
!         (i-1,j  )        (i  ,J  )      (i+1,j  )
!                        point to fill
!
!         (i-1,j-1)        (i  ,J-1)      (i+1,j-1)
!
implicit none

integer ni,nj
real, dimension(ni,nj) :: datafield
integer, dimension(ni,nj) :: mask

integer, dimension(ni,nj,0:1) :: tempmask
integer inmask,outmask
integer i,j,ii,jj
integer not_filled, neighbors, not_filled_on_last_pass
real *8 sum, sumweight, weight

tempmask(:,:,0)=0
do j=1,nj  ! loop over input grid
do i=1,ni
  if ( mask(i,j) .gt. 0.5 ) tempmask(i,j,0)=1
end do
end do

!tempmask(:,:,0)=mask
inmask=0
outmask=1-inmask  ! if inmask is 0, then outmask is 1 and vice versa
not_filled=ni*nj-1
not_filled_on_last_pass=ni*nj

do while(not_filled.ne.0)   ! loop until there are no missing values left
  not_filled_on_last_pass=not_filled
  not_filled=0
  do j=1,nj  ! loop over input grid
  do i=1,ni
    tempmask(i,j,outmask)=tempmask(i,j,inmask)
    if(tempmask(i,j,inmask).ne.0) cycle ! this point exists, no need to fill
    if(tempmask(i,j,inmask).eq.1) print *,'point',i,j,' exists'
    sum=0.0
    sumweight=0.0
    neighbors=0
    do jj=max(1,j-1),min(nj,j+1)              ! scan neighbors (with protection for border)
    do ii=max(1,i-1),min(ni,i+1)
      if(ii.ne.i .or. jj.ne.j) then           ! ignore the point itself
        if(tempmask(ii,jj,inmask).ne.0) then  !  point is not missing in grid
          weight=1.0/sqrt( 1.0*((ii-i)*(ii-i) + (jj-j)*(jj-j)) )   ! weight is inverse of distance in grid points
          sumweight=sumweight+weight          ! sum of weights
          sum=sum+weight*datafield(ii,jj)         ! weighted sum
          neighbors=neighbors+1
        endif
      endif
    enddo
    enddo
    if(neighbors .gt. 1) then     ! we need at least two existing neighboring points
      datafield(i,j)=sum/sumweight    ! weighted average of existing neighbors
      tempmask(i,j,outmask)=1     ! this point now exists (for next pass)
    else
      not_filled=not_filled+1             ! this point could not be filled
    endif
10 continue
  enddo
  enddo
  inmask=outmask    ! swap inmask and outmask (this output is the input for next pass)
  outmask=1-inmask  ! if inmask is 0, then outmask is 1 and vice versa

!  print *,'not_filled=',not_filled
  if(not_filled.eq.not_filled_on_last_pass) exit   ! we are stuck, no missing values got filled on this pass
enddo

oil_spill_fill=not_filled   ! return number of holes left

101 format(8f6.3)
return
end

