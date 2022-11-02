      program oil_spill_SST_SeaIce

! s.compile -src oil_spill_SST_SeaIce.f90 -o oil_spill_SST_SeaIce.Abs -librmn rmn_015

!     mask=/BIG2/data/SST_SeaIce_degK/ERA5/ls_mask_SST_SeaIce_era5
!    ~winger/Scripts/ERA5/SST_SeaIce/oil_spill_SST_SeaIce.Abs -s ${tmp1} -d ${RPN_file} -mask $mask -etiket ${etiket}

      implicit none

!     Declarations for CCARDs
      integer       nkey
      parameter   ( nkey = 4 )
      CHARACTER*8   cles(nkey)
      character*128 nam(nkey),def(nkey), dummy
      integer       npos

!     Include external functions
      integer  fnom, fstouv, fclos, fstfrm, wkoffit, longueur
      integer  fstinf, fstinfx, fstluk, fstprm, fstecr, fstinl
      external fnom, fstouv, fclos, fstfrm, wkoffit, longueur
      external fstinf, fstinfx, fstluk, fstprm, fstecr, fstinl

!     Variables needed to read field descriptions (fstprm)
      integer h, handle, ni, nj, nk, dgid
      integer dateo,deet,npas,nbits,datyp
      integer ip1,ip2,ip3,swa,lng,dltf,ubc
      integer ig1,ig2,ig3,ig4,extra1,extra2,extra3
      character*1  grtyp
      character*2  typvar
      character*4 nomvar
      character*12 etiket

      character*512 ifile, mfile, ofile

      integer iun, mun, oun, ier, ftyp

      real,    dimension (:,:), allocatable :: field, mask

      integer holes_left, oil_spill_fill
      integer i,j, handle_LG
      character*12 new_etiket

      DATA cles /'S.' ,'D.' ,'MASK.','etiket'/
      DATA def  /'   ','   ','   '  ,     ' '/
      DATA nam  /'   ','   ','   '  ,     ' '/

!     Read input parameters
      npos = -1    
      CALL CCARD(CLES,DEF,NAM,nkey,npos)


!     Open input file
      ifile = nam(1)
      iun   = 0
      ier   = fnom(iun, ifile, 'STD+RND+R/O', 0)
!     Check file type of input file
      ftyp  = WKOFFIT(ifile)
      if (ftyp.ne.1.and.ftyp.ne.2.and.ftyp.ne.33.and.ftyp.ne.34) then
        print *,'Wrong input file type, ABORT!'
        stop
      end if
      ier = fstouv(iun,'STD+RND')
      print *,' ifile = ', ifile(1:longueur(ifile))

!     Open mask
      mfile = nam(3)
      mun   = 0
      ier   = fnom(mun, mfile, 'STD+RND+R/O', 0)
!     Check file type of input file
      ftyp  = WKOFFIT(mfile)
      if (ftyp.ne.1.and.ftyp.ne.2.and.ftyp.ne.33.and.ftyp.ne.34) then
        print *,'Wrong mask file type, ABORT!'
        stop
      end if
      ier = fstouv(mun,'STD+RND')
      print *,' mfile = ', mfile(1:longueur(ifile))

!     Open output file
      ofile = nam(2)
      oun   = 0
      ier   = fnom(oun, ofile, 'STD+RND', 0)
      ier   = fstouv(oun,'STD+RND')
      print *,' ofile = ', ofile(1:longueur(ifile))

!     Read other input parameters
      new_etiket = nam(4)


! -------------------------------------------------------------------

      i=242; j=144
      i=51 ; j=52
      i=756; j=314 !300.614 K

!     Read mask
      handle = fstinf(mun, ni,nj,nk, -1,' ',-1,-1,-1,' ', ' ')
!      print *,'ni,nj:',ni,nj
      allocate (mask(ni,nj))
      ier = fstluk(mask, handle, ni, nj, nk)
!      print *,'mask(i,j):',mask(i,j)

!    
      handle = 0
!     Loop over all 'TM' records in field
100   continue
!       Get handle of next record 
        handle = fstinfx(handle, iun, ni,nj,nk, -1, ' ', &
                         -1,-1,-1, ' ', 'TM')

!       If no record is found leave loop
        if ( handle .lt. 0 ) goto 101

        allocate (field(ni,nj))
        ier = fstprm(handle,dateo,deet,npas,ni,nj,nk,nbits, &
              datyp,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2, &
              ig3,ig4,swa,lng,dltf,ubc,extra1,extra2,extra3)

!      print *,'nomvar:',nomvar

!       Read field
        ier = fstluk(field, handle, ni, nj, nk)
      print *,'field(i,j):',field(i,j)
!      print *,'field:',field
!      stop

!        if ( nomvar .ne. '^^' .and. &
!             nomvar .ne. '>>' .and. &
!             nomvar .ne. 'HY' ) then

          holes_left=oil_spill_fill(field,ni,nj,mask)
          print *,'There are',holes_left,' holes left in array'

          nbits=16
          datyp=134

!        end if

        if ( new_etiket .ne. ' ' ) etiket = new_etiket
        typvar = 'A'
        ip1    = 0

!       Write record into RPN Standard output file
        ier = fstecr(field,field, &
                     -nbits,oun,dateo,deet,npas,ni,nj,nk, &
                     ip1,ip2,ip3,typvar,nomvar,etiket, &
                     grtyp,ig1,ig2,ig3,ig4,datyp,.false.)

!       Now treat LG record for the same time step
        handle_LG = fstinf(iun, ni,nj,nk, dateo, ' ', &
                         -1,-1,-1, ' ', 'LG')
        ier = fstluk(field, handle_LG, ni, nj, nk)
        holes_left=oil_spill_fill(field,ni,nj,mask)
        print *,'There are',holes_left,' holes left in array'
        ier = fstecr(field,field, &
                     -nbits,oun,dateo,deet,npas,ni,nj,nk, &
                     ip1,ip2,ip3,typvar,'LG',etiket, &
                     grtyp,ig1,ig2,ig3,ig4,datyp,.false.)

        
        deallocate (field)

        goto 100
101   continue

!     Close RPN Standard files
      ier = fstfrm(iun)
      ier = fclos (iun)
      ier = fstfrm(oun)
      ier = fclos (oun)
      ier = fstfrm(mun)
      ier = fclos (mun)

end

! ====================================================================

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
real, dimension(ni,nj) :: mask

integer, dimension(ni,nj,0:1) :: tempmask
integer inmask,outmask
integer i,j,ii,jj,iii,jjj
integer not_filled, neighbors, not_filled_on_last_pass
real *8 sum, sumweight, weight
integer, parameter :: R=1

!print *,'In oil_spill_fill'

tempmask(:,:,0)=0
do j=1,nj  ! loop over input grid
do i=1,ni
  if ( mask(i,j) .gt. 0.5 ) tempmask(i,j,0)=1
end do
end do

!print *,'tempmask:',tempmask(:,:,0)
!print *,'tempmask(1,1):',tempmask(1,1,0)
!print *,'datafield:',datafield
!stop

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
    do jj=max(1,j-R),min(nj,j+R)              ! scan neighbors (with protection for border)
    do ii=i-R,i+R                             ! scan neighbors (going over the border)

      iii=ii
      jjj=jj
      if (ii.le. 0) iii=ni-ii
      if (ii.gt.ni) iii=ii-ni
!      if (jj.le. 0) jjj=nj-jj
!      if (jj.gt.nj) jjj=jj-nj

!if (i.eq.1 .and. j.eq.1) then
!  print *,'Treat point 1,1'
!  print *,'iii,jjj:',iii,jjj
!end if

      if(iii.ne.i .or. jjj.ne.j) then           ! ignore the point itself
        if(tempmask(iii,jjj,inmask).ne.0) then  !  point is not missing in grid
          weight=1.0/sqrt( 1.0*((ii-i)*(ii-i) + (jj-j)*(jj-j)) )   ! weight is inverse of distance in grid points
          sumweight=sumweight+weight          ! sum of weights
          sum=sum+weight*datafield(iii,jjj)         ! weighted sum
          neighbors=neighbors+1
        endif
      endif
    enddo
    enddo
    if(neighbors .gt. 1) then     ! we need at least two existing neighboring points
!if (i.eq.1 .and. j.le.2) then
!  print *,'Point filed:',i,j
!end if

!print *,'One point filled',i,j
!print *,'Before:',datafield(i,j)
      datafield(i,j)=sum/sumweight    ! weighted average of existing neighbors
!print *,'After :',datafield(i,j)
!stop
      tempmask(i,j,outmask)=1     ! this point now exists (for next pass)
    else
      not_filled=not_filled+1             ! this point could not be filled
    endif
10  continue
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

