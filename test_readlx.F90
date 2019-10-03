! useful routines for C and FORTRAN programming
! Copyright (C) 1975-2019  Meteorological Research Branch
!                          Environnement Canada
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
module readlx_test
  integer, parameter :: NTAB = 10
  integer, dimension(NTAB), save :: TAB1, TAB2, TAB3, INDICE
  integer, save :: NTAB1, NTAB2, NTAB3, NSUB1, NSUB2, NSUB3, NNN, NSUBD
end module
      PROGRAM READLXTEST
      use readlx_test
      INTEGER dummy

      EXTERNAL SUB1, SUB2, YOUPI, DIAG
!       integer*8 loc_sub
!       external loc_sub

!      IER = FNOM(6,'$OUTPUT','SEQ',0)
      CALL qlxins(TAB1,   'TAB1',    NTAB1, 9, 1)
      CALL qlxins(TAB2,   'TAB2',    NTAB2, 4, 1)
      CALL qlxins(TAB3,   'TAB3',    NTAB3, 7, 1)
      CALL qlxins(INDICE, 'IND',    NNN,   103, 1)
      CALL qlxins(22,     'CONST22', dummy, 1, 0)
      CALL qlxins(55,     'CONST55', dummy, 1, 0)
print 111,loc(TAB1),loc(TAB2),loc(TAB3)
111 format(3z20.16)

      CALL qlxinx(SUB1, 'SUB1', NSUB1, 0104, 2)
      CALL qlxinx(SUB2, 'SUB2', NSUB2, 0305, 2)
      CALL qlxinx(YOUPI,'SUB3', NSUB3, 0000, 2)
      CALL qlxinx(DIAG ,'DIAG', NSUBD, 0000, 2)
!
!       write(*,77) loc_sub(sub1),loc_sub(sub2)
!  77   format(' *** debug sub1 = ',z16.16,' sub2 = ',z16.16)
      PRINT *,' Avant READLX - input=inp_readlx'
!       IER = FNOM(5,'inp_readlx_simple','SEQ',0)
      CALL READLX(5,KND,KRR)
      PRINT *,' APRES READLX - KND,KRR, ind, indice ',KND,KRR,NNN,INDICE
!     CALL READLX(5,KND,KRR)
!     PRINT *,' APRES READLX - KND,KRR ',KND,KRR

!     PRINT *,' NTAB1,NTAB2MNTAB3 -',NTAB1,NTAB2,NTAB3
    WRITE(6,'(3X,4Z20)')TAB1
    WRITE(6,'(3X,4Z20)')TAB2
    WRITE(6,'(3X,4Z20)')TAB3
      STOP
      END
      SUBROUTINE SUB1(A,B,C,D)
      use readlx_test
      INTEGER A(*),B(*),C(*),D(*),ARGDIMS,ARGDOPE,ND
      INTEGER LISTE(5)
      PRINT *,' PASSE PAR SUB1'
      PRINT *,' NB D ARGUMENTS =',NSUB1
      GOTO(1,2,3,4)NSUB1
4     PRINT 101,D(1),D(1),LOC(D),(D(I),I=1,ARGDIMS(4))
      LISTE = -1
      ND = ARGDOPE(4,LISTE,5)
      PRINT *,'ARGDIM =',ARGDIMS(4)
      PRINT 102,ND,LISTE
3     PRINT 101,C(1),C(1),LOC(C),(C(I),I=1,ARGDIMS(3))
      LISTE = -1
      ND = ARGDOPE(3,LISTE,5)
      PRINT *,'ARGDIM =',ARGDIMS(3)
      PRINT 102,ND,LISTE
2     PRINT 101,B(1),B(1),LOC(B),(B(I),I=1,ARGDIMS(2))
      LISTE = -1
      ND = ARGDOPE(2,LISTE,5)
      PRINT *,'ARGDIM =',ARGDIMS(2)
      PRINT 102,ND,LISTE
1     PRINT 101,A(1),A(1),LOC(A),(A(I),I=1,ARGDIMS(1))
      LISTE = -1
      ND = ARGDOPE(1,LISTE,5)
      PRINT *,'ARGDIM =',ARGDIMS(1)
      PRINT 102,ND,LISTE
101   FORMAT(3X,Z20.8,I10,5Z20.8)
102   FORMAT(1X,' ND=',I2,' LISTE=',5Z12.8)
      END
      SUBROUTINE SUB2(A,B,C,D,E)
      use readlx_test
      INTEGER A,B,C,D,E
      PRINT *,' PASSE PAR SUB2'
      PRINT *,' NB D ARGUMENTS =',NSUB2
      GOTO(1,2,3,4,5)NSUB2
5     PRINT *,E,LOC(E)
4     PRINT *,D,LOC(D)
3     PRINT *,C,LOC(C)
2     PRINT *,B,LOC(B)
1     PRINT *,A,LOC(A)
      RETURN
      END
      SUBROUTINE YOUPI
      PRINT *,' <><><> Y O U P I <><><>'
      RETURN
      END
      SUBROUTINE DIAG
      use readlx_test
      PRINT *,'NTAB1, NTAB2, NTAB3 =',NTAB1, NTAB2, NTAB3
      print 100,'tab1 =',tab1(1:NTAB1)
      print 100,'tab2 =',tab2(1:NTAB2)
      print 100,'tab3 =',tab3(1:NTAB3)
100   format(A,20I6)
      RETURN
      END
