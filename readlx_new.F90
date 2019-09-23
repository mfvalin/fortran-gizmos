
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
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this software; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
! 
!

!
!  MACRO ICHAMP(MOT,FIN,LG)   EXTRAIT UN CHAMP D'UN MOT
!
!   MOT   MOT QUI CONTIENT LE CHAMP
!   FIN   NUMERO DU DERNIER BIT (A DROITE) DU CHAMP, EN NUMEROTATION
!         GAUCHE > DROITE (LE BIT 0 EST A DROITE DU MOT).
!   LG    LONGUEUR, EN BITS, DU CHAMP
!

!
!  MACRO IUNPAK(BASE, BITPOS, LG)  OBTENIR UN CHAMP D'UN TABLEAU
!
!  BASE    TABLEAU CONTENANT LE CHAMP A EXTRAIRE
!  BITPOS  POSITION DU BIT DE DROITE DU CHAMP A EXTRAIRE
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU.
!  LG      EST LE NOMBRE DE BITS QU'OCCUPE LE CHAMP. (MAX 32 BITS)
!

!
!
!  MACRO GETBIT(BASE, BITPOS, LG)  OBTENIR UN CHAMP D'UN TABLEAU
!
!  BASE    TABLEAU CONTENANT LE CHAMP A EXTRAIRE
!  BITPOS  POSITION DU BIT DE DROITE DU CHAMP A EXTRAIRE
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU.
!  LG      EST LE NOMBRE DE BITS QU'OCCUPE LE CHAMP. (MAX 32 BITS)
!

!
!
!  MACRO INSERT(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU
!
!  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
!  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
!  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
!  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)
!

!
!  MACRO PUTBIT(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU
!
!  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
!  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
!  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
!  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)
!

!
!  MACRO CLRBIT(TABL,BITPOS,LONG)  METTRE A ZERO UN CHAMP DANS UN TABLEAU
!
!  TABL    TABLEAU
!  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A NETTOYER
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
!  LONG    LONGUEUR EN BIT DU CHAMP A NETTOYER (PAS PLUS DE 32 BITS)
!

!
!  MACRO PUTBITC(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU
!                                       AVEC NETTOYAGE PRELIMINAIRE
!  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
!  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
!  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
!          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
!  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)
!
! TODO:
! QLXDTYP:  TO BE REVISITED, POINTLESS THE WAY IT IS BEING USED
!
module readlx_internals
  implicit none
!   COMMON /PARMADR/NPRM,NARG,DOPE(41),PARM(101)
!   COMMON /PARMADR/NDOPES,DOPEA(42),DOPES(101),ADR(41)
!   INTEGER NARG,NPRM,DOPE,NDOPE,DOPEA,DOPES,PARM
!   Integer*8 ADR
  integer, parameter :: MAX_ARGS = 40    ! max number of arguments
  integer, parameter :: MAX_ARGL = 100   ! max number of items for ALL arguments
  integer, save      :: NPRM = 0
  integer, save      :: NARG = 0         ! number of arguments for current call
  integer, save      :: NDOPES = 0
  ! argument K starts at DOPES(DOPEA(K)), ends at DOPES(DOPEA(K+1)-1)
  ! the length of argument K is DOPE(K)
  integer, save, dimension(MAX_ARGS+2)         :: DOPEA       ! start index of arguments in DOPES
  integer, save, dimension(MAX_ARGS+1)         :: DOPE        ! number of items in arguments
  integer, dimension(MAX_ARGL+1), save         :: DOPES       ! all argument values
  integer, dimension(MAX_ARGL+1), save         :: PARM = 0
  integer(kind=8), dimension(MAX_ARGS+1), save :: ADR = 0

!   type = 0 alphanumeric token <= 8 chars
!   type = 1 integer number
!   type = 2 real number
!   type = 3 alphanumeric token > 8 chars (STRING)
!   type = 4 operator
!   type = 5 bad number
!   type = 6 octal number
!   COMMON/QLXTOK1/LEN,TYPE,ZVAL,INEXPR
!   LOGICAL INEXPR
!   INTEGER LEN,TYPE,JVAL
!   REAL ZVAL
!   EQUIVALENCE (ZVAL,JVAL)
  LOGICAL, save :: INEXPR
  INTEGER, save :: LEN,TYPE,JVAL
  REAL :: ZVAL
  EQUIVALENCE (ZVAL,JVAL)
! 
!   COMMON/QLXTOK2/TOKEN
!   CHARACTER(len=80) :: TOKEN
  CHARACTER(len=80), save :: TOKEN
! 
!   CHARACTER(len=20) :: LINEFMT
!   INTEGER KARMOT
!   COMMON /QLXFMT/ LINEFMT
!   COMMON /QLXFMT2/ KARMOT
  CHARACTER(len=20), save :: LINEFMT   ! format used to read a line of input into an array
  INTEGER, save :: KARMOT = 4

!       COMMON /QLXBUFF/ NC,LAST,INPFILE,EOFL,NERR,SKIPFLG
!       COMMON /QLXBUFF/ CURREC,READREC,TMPFILE
!       INTEGER NC,LAST,INPFILE,NERR,SKIPFLG,CURREC,READREC,TMPFILE
!       LOGICAL EOFL
      INTEGER, save :: NERR,SKIPFLG,CURREC,READREC,TMPFILE
      INTEGER, save :: NC = 1
      INTEGER, save :: LAST = 0
      INTEGER, save :: INPFILE = 5
      LOGICAL, save :: EOFL = .false.
!       COMMON /QLXBUF2/ INLINE
!       CHARACTER(len=101) :: INLINE
      CHARACTER(len=101), save :: INLINE = ' '   ! read into position 21 -> end, first 20 positions kept for pushback
!       DATA NC,LAST/1,0/
!       DATA INPFILE/5/
!       DATA EOFL/.FALSE./
!       DATA INLINE/' '/

!       INTEGER ITAB(3:3,256),NENTRY
!       Integer(kind=8), dimension(2,256) :: IPTADR
!       CHARACTER *8 NAMES(256)
        character(len=8), dimension(256), save :: NAMES = ' '
        integer, dimension(3:3,256), save :: ITAB = 0
        Integer(kind=8), dimension(2,256), save :: IPTADR = 0
        integer, save :: NENTRY = 0
!       COMMON /qqq_nrdlx/ NAMES, ITAB, NENTRY
!       COMMON /qqq_nrdlx2/ IPTADR
!       
!       DATA ITAB /256 * 0/
!       DATA IPTADR /256 * 0,256 * 0/
!       DATA NAMES /256 * ' '/
!       DATA NENTRY /0/
end module
!
!**FONCTION ARGDIMS LONGUEUR D'ARGUMENTS (APPEL VIA READLX)
!
      INTEGER FUNCTION ARGDIMS(N)  ! return length of argument N
      use readlx_internals
      implicit none
      INTEGER, intent(IN) :: N
!
!OBJET(ARGDIMS)
!         RENVOYER LA LONGUEUR EN NOMBRE DE MOTS DE L'ARGUMENT
!         N DU DERNIER APPEL EFFECTUE VIA READLX
!
!ARGUMENTS
! IN      N     NUMERO D'ORDRE DE L'ARGUMENT DANS LA LISTE
!
!IMPLICITES
!       COMMON /PARMADR/NPRM,NARG,DOPE(41),PARM(101)
!       COMMON /PARMADR/NDOPES,DOPEA(42),DOPES(101),ADR(41)
!       INTEGER NARG,NPRM,DOPE,NDOPE,DOPEA,DOPES,PARM
!       Integer*8 ADR
!*
      IF((N .LE. NARG))THEN
         ARGDIMS = DOPE(N)
      ELSE 
         ARGDIMS = 0       ! N > number of arguments for this call
      ENDIF 
      RETURN
      END
!
!**FONCTION ARGDOPE - GET DOPE LIST OF ARGUMENT NARG
!
      INTEGER FUNCTION ARGDOPE(N,LISTE,ND)   ! return number of items in argument N
      use readlx_internals
      implicit none
      INTEGER, intent(IN)  :: N,ND
      INTEGER, intent(OUT) :: LISTE(ND)      ! argument array returned to caller
!
!
!OBJET(ARGDOPE)
!      GET DOPE LIST OF ARGUMENT NARG
!
!AUTEUR
!     M. VALIN
!
!IMPLICITE
!
!       COMMON /PARMADR/NPRM,NARG,DOPE(41),PARM(101)
!       COMMON /PARMADR/NDOPES,DOPEA(42),DOPES(101),ADR(41)
!       INTEGER NARG,NPRM,DOPE,NDOPE,DOPEA,DOPES,PARM
!       Integer*8 ADR
!
!*
      INTEGER I,BASE

      IF( (N.GT. NARG))THEN
         ARGDOPE = 0       ! N > number of arguments for this call
      ELSE 
         BASE = DOPEA(N)                             ! first index
         ARGDOPE = DOPEA(N+1) - DOPEA(N)             ! length
         DO 23002 I = 1,MIN(DOPEA(N+1)-DOPEA(N),ND)  ! get values for this argument (max of ND values)
            LISTE(I) = DOPES(BASE+I-1)
23002    CONTINUE 
      ENDIF 
      RETURN
      END
!
!**S/P LEXINS  -  INTERFACE DE QLXINS
!
      SUBROUTINE LEXINS(IVAR,ICLE,NB,LIMIT,TYP)  ! legacy support of an old routine
      implicit none
      INTEGER IVAR,ICLE,NB,LIMIT,TYP
!
!AUTEUR M. LEPINE  -  OCT 89
!
!OBJET(LEXINS)
!     INTERFACE ENTRE L'ANCIENNE ROUTINE LEXINS ET LA NOUVELLE
!     QLXINS
!*
      CHARACTER * 8 KLE
!
      WRITE(KLE,'(A8)') ICLE
      CALL QLXINS(IVAR,KLE,NB,LIMIT,TYP)
      RETURN
      END
!
!**S/P QLXADI GET VALUE OF INDEXED ARRAY COMPONENT  (ONLY USED BY QLXVAL)
      SUBROUTINE QLXADI(KLE,IND,VALEUR,dummy,ERR)  ! in case of error, VALEUR is undefined
      implicit none
      INTEGER, intent(IN)  :: IND           ! index (origin 1)
      INTEGER, intent(OUT) :: VALEUR        ! value returned to user
      INTEGER, intent(IN)  :: dummy         ! not used
      LOGICAL, intent(OUT) :: ERR           ! error flag
      CHARACTER(len=*), intent(IN) :: KLE   ! key (name of array or value)
!*
      INTEGER, EXTERNAL :: QLXDTYP
      Integer(kind=8) ::  LOCVAR,LOCCNT
      INTEGER :: LIMITE,ITYP,IZ,INDX
      REAL :: Z
!       EQUIVALENCE(Z,IZ)
      IZ = IND
      Z  = transfer(IND,Z)
      ERR = .false.
      IF((QLXDTYP(IZ).EQ.1))THEN   ! is it integer ?  (almost always true if integers are 32 bit values)
         INDX = IZ                 ! use integer value as index
      ELSE                         ! it is real
         INDX = NINT(Z)            ! index must be integre, take nearest integer value if float
      ENDIF 
      CALL QLXFND(KLE,LOCVAR,LOCCNT,LIMITE,ITYP)  ! find key in table
      IF((ITYP.NE.0 .AND. ITYP.NE.1))THEN
         ERR = .TRUE.
      ENDIF 
      IF((INDX.GT.LIMITE .OR. INDX.LE.0))THEN     ! index is OUT OF BOUNDS
         ERR = .TRUE.
      ENDIF 
      IF((.NOT.ERR))THEN
         CALL PEEK(LOCVAR,INDX,VALEUR)  ! get 32 bit value at locvar + INDX -1 elements
      ENDIF 
      RETURN
      END
!
!*S/P GET OPTIONAL SUBSCRIPT THEN BUILD MEMORY ADDRESS
      Integer(kind=8) FUNCTION QLXADR(KLE,ERR) ! get base address of key (symbol), return 0 if error
      implicit none
      CHARACTER(len=*), intent(IN) :: KLE
      LOGICAL, intent(OUT) :: ERR
      INTEGER :: LIMITS,ITYP,IND
      Integer(kind=8) :: LOCCNT, locvar
      Integer(kind=8), external :: get_address_from
      integer, dimension(*) :: VARI
      POINTER (P,VARI)
!*
      CALL QLXIND(IND,ERR)  ! get possible index value (1 will be returned if none found)
!
      IF((.NOT. ERR))THEN
         CALL QLXFND(KLE,locvar,LOCCNT,LIMITS,ITYP)               ! find base address of key (symbol)
         call make_cray_pointer(P,locvar)                         ! make cray pointer from LOCVAR
         IF((IND.LE.LIMITS .AND. ITYP.GE.0 .AND. ITYP.LE.1))THEN
            QLXADR = get_address_from(VARI(IND))      ! loc(vari(ind)) would probably be O.K.
         ELSE 
            ERR=.TRUE.
            CALL QLXERR(21017,'QLXADR')
            QLXADR=0
         ENDIF 
      ELSE 
         QLXADR=0
      ENDIF 
      RETURN
      END

!
!**S/P QLXASG ASSIGNATION D'UNE OU PLUSIEURS VALEURS
      SUBROUTINE QLXASG(VAL,ICOUNT,LIMIT,ERR)   ! process assignment statement
      use readlx_internals
      implicit none
      integer(kind=8), intent(IN) :: VAL           ! assignment target MEMORY ADDRESS
      INTEGER, intent(OUT)        :: ICOUNT        ! number of values stored
      INTEGER, intent(IN)         :: LIMIT         ! max number of values that may be stored
      LOGICAL, intent(OUT)        :: ERR           ! error flag
!
!OBJET(QLXASG)
!        PREND LES TOKENS QUI SUIVENT LE SIGNE  =  ET SEPARES PAR DES VIRGULES
!        POUR LES PLACER A L'ADRESSE VAL. ICOUNT EST LE NOMBRE DE MOTS DEPOSES
!ARGUMENTS
! E      VAL     ADRESSE DE LA CLE CIBLE
! E      ICOUNT  NOMBRE DE MOTS DEPOSES
! E      LIMIT   NOMBRE MAXIMAL DE MOTS QUE VAL PEUT ACCUEILLIR
! S      ERR     INDICATEUR D'ERREUR
!
!IMPLICITES

!       COMMON/QLXTOK1/LEN,TYPE,ZVAL,INEXPR
!       LOGICAL INEXPR
!       INTEGER LEN,TYPE,JVAL
!       REAL ZVAL
!       EQUIVALENCE (ZVAL,JVAL)
! !
! 
!       COMMON/QLXTOK2/TOKEN
!       CHARACTER(len=80) :: TOKEN
! !
! 
!       CHARACTER(len=20) :: LINEFMT
!       INTEGER KARMOT
!       COMMON /QLXFMT/ LINEFMT
!       COMMON /QLXFMT2/ KARMOT
!*
      INTEGER IND,JLEN,QLXVAL
      INTEGER OLDTYP,ITEMP(80),IREPCN
      REAL TEMP(80)
      EQUIVALENCE (TEMP,ITEMP)
      LOGICAL IAREP,FIN
      integer :: i, j
!
      IND=1
      OLDTYP=4
      FIN=.FALSE.
      IAREP=.FALSE.
      IREPCN=1
      JLEN=0
      CALL QLXIND(IND,ERR)

      IF((.NOT.ERR))THEN
         CALL QLXTOK
      ENDIF 
      IF((TOKEN(1:2).EQ.'= ' .AND. TYPE.EQ.4 .AND. .NOT. ERR))THEN  ! = operator
23004    IF((.NOT.ERR .AND. .NOT.FIN))THEN                          ! while not done or error
            CALL QLXTOK
            IF( ((TYPE.EQ.4) .AND. (TOKEN(1:1).EQ.'(')))THEN        ! starts with (, analyze expression
#if defined(WITH_EXPRESSIONS)
               CALL QLXXPR(ERR)
#else
               ERR = .true.
#endif
               IF((ERR))THEN
                  GOTO 23005
               ENDIF 
            ENDIF 
            IF((TYPE.EQ.8))THEN    ! expression result (from qlxxpr)
               call get_content_of_location(JVAL,1,JVAL)    ! this is BROKEN, it will not work if JVAL is 32 bit value
            ELSE 
               IF((TYPE.EQ.1 .AND. OLDTYP.EQ.4))THEN  ! integer following operator
                  ITEMP(1)=JVAL
                  JLEN=1
!
               ELSE 
                  IF((TYPE.EQ.2 .AND. OLDTYP.EQ.4))THEN ! float following operator
                     TEMP(1)=ZVAL
                     JLEN=1
!
                  ELSE 
                     IF((TYPE.EQ.3 .AND. OLDTYP.EQ.4))THEN   ! long alphanumeric token following operator
                        JLEN=(LEN+KARMOT-1)/KARMOT
                        READ(TOKEN,LINEFMT)(ITEMP(J),J=1,JLEN)  ! get a bunch of KARMOT characters into an integer array
!
                     ELSE 
                        IF((TYPE.EQ.4))THEN                     ! operator
                           IF((TOKEN(1:2).EQ.'% '))THEN
                              IF((OLDTYP.EQ.1 .AND.(.NOT.IAREP)))THEN
                                 IREPCN=ITEMP(1)
                                 IF((IREPCN.GT.0))THEN
                                    IAREP=.TRUE.
                                    JLEN=0
                                 ELSE 
                                    CALL QLXERR(21001,'QLXASG')
                                    ERR=.TRUE.
                                 ENDIF 
                              ELSE 
                                 CALL QLXERR(21002,'QLXASG')
                                 ERR=.TRUE.
                              ENDIF 
!
                           ELSE 
                              IF((TOKEN(1:2).EQ.', ' .OR.TOKEN(1:2).EQ.'$ '))THEN  ! comma or end of line
                                 IF(((IREPCN*MAX(JLEN,1)+IND).GT.LIMIT+1))THEN
                                    CALL QLXERR(21003,'QLXASG')
                                    ERR=.TRUE.
                                 ELSE 
                                    DO 23030  I=1,IREPCN
                                       DO 23032  J=1,JLEN
                                          call set_content_of_location(VAL,IND+J-1,ITEMP(J))
23032                                  CONTINUE 
                                       IND=IND+MAX(JLEN,1)
23030                               CONTINUE 
                                    IREPCN=1
                                    IAREP=.FALSE.
                                    JLEN=0
                                    ICOUNT = IND-1
                                 ENDIF 
                                 FIN=TOKEN(1:1).EQ.'$'
                              ELSE 
                                 CALL QLXERR(21004,'QLXASG')
                                 ERR=.TRUE.
                              ENDIF 
                           ENDIF 
!
                        ELSE 
                           IF((TYPE.EQ.0 .AND. OLDTYP.EQ.4))THEN
                              JLEN=1
                              ITEMP(1)=QLXVAL(TOKEN(1:8),ERR)
                           ELSE 
                              CALL QLXERR(21005,'QLXASG')
                              ERR=.TRUE.
                           ENDIF 
                        ENDIF 
                     ENDIF 
                  ENDIF 
               ENDIF 
            ENDIF 
            OLDTYP=TYPE
            GOTO 23004
         ENDIF   ! end while
23005    CONTINUE 
!
      ELSE ! not assignation operator, OOPS !!
         CALL QLXERR(21006,'QLXASG')
         ERR=.TRUE.
!
      ENDIF 
      RETURN
      END
!
!**S/P QLXBAK     RENVOYER UN CARACTERE
      SUBROUTINE QLXBAK(ICAR)  ! push character back into input buffer
      use readlx_internals
      implicit none
      CHARACTER(len=1), intent(IN) :: ICAR
!
!
!AUTEUR   M. VALIN  RPN  JUIN 1983
!
!OBJET(QLXBAK)
!        QLXBAK REMET UN CARACTERE DANS UNE LIGNE DE TEXTE,
!        A LA POSITION COURANTE. IL RECULE EN CONSEQUENCE LE
!        POINTEUR DU CARACTERE COURANT
!ARGUMENT
!        ICAR      CARACTERE(1 CARACTERE HOLLERITH) RENVOYE DANS LA LIGNE DE TEX
!         E
!
! 
!       COMMON /QLXBUFF/ NC,LAST,INPFILE,EOFL,NERR,SKIPFLG
!       COMMON /QLXBUFF/ CURREC,READREC,TMPFILE
!       INTEGER NC,LAST,INPFILE,NERR,SKIPFLG,CURREC,READREC,TMPFILE
!       LOGICAL EOFL
!       COMMON /QLXBUF2/ INLINE
!       CHARACTER(len=101) :: INLINE
!*
      IF((NC.GT.1))THEN
         INLINE(NC-1:NC-1)=ICAR
         NC=NC-1
      ELSE 
         CALL QLXERR(81007,'QLXBAK')  ! OUCH !! no room to push character back
!
      ENDIF 
      RETURN
      END
!
      SUBROUTINE QLXCALL(SUB,ICOUNT,LIMITS,ERR)   ! process a call directive NAME(parm,parm,....,parm)
      use readlx_internals
      implicit none
      Integer(kind=8) :: SUB        ! address of subroutine to call
      Integer(kind=8) :: ICOUNT     ! number of arguments
      integer, intent(IN) :: limits ! min and max acceptable number of arguments (maxargs + 100 * minargs)
      logical, intent(OUT) :: ERR
!
      Integer(kind=8), external :: get_address_from

!       COMMON/QLXTOK1/LEN,TYPE,ZVAL,INEXPR
!       LOGICAL INEXPR
!       INTEGER LEN,TYPE,JVAL
!       REAL ZVAL
!       EQUIVALENCE (ZVAL,JVAL)
! !
! 
!       COMMON/QLXTOK2/TOKEN
!       CHARACTER *80 TOKEN
!
!

!       COMMON /PARMADR/NPRM,NARG,DOPE(41),PARM(101)
!       COMMON /PARMADR/NDOPES,DOPEA(42),DOPES(101),ADR(41)
!       INTEGER NARG,NPRM,DOPE,NDOPE,DOPEA,DOPES,PARM
!       Integer*8 ADR
!       CHARACTER * 20 LINEFMT
!       INTEGER KARMOT
!       COMMON /QLXFMT/ LINEFMT
!       COMMON /QLXFMT2/ KARMOT
!

      EXTERNAL RMTCALL, QLXADR, QLXVAL
      INTEGER  RMTCALL, QLXVAL
      INTEGER LIM1,LIM2,JLEN,PREVI
      Integer(kind=8) :: LOCDUM, QLXADR
      CHARACTER(len=8) :: KLE
      integer :: i, j, JUNK, NPRM0
!
      LOGICAL FIN,INLIST
!
!       DATA ADR  /41*0/
!       DATA PARM /101*0/
!
      FIN  = .FALSE.
      INLIST = .FALSE.
      LOCDUM =get_address_from(PARM(1))
      NDOPES = 0
      DO 23000 I = 1,41
         DOPE(I) = 0
         DOPEA(I) = 0
         ADR(I) = LOCDUM
23000 CONTINUE 
      NARG = 0
      NPRM = 0
      NPRM0 = 0
      PREVI =4
!
      CALL QLXTOK
      IF( (TYPE.NE.4 .AND. TOKEN(1:1).NE.'('))THEN  ! MUST BE ( at strat of call
         CALL QLXERR(81018,'QLXCALL')
         ERR = .TRUE.
!
      ENDIF 
23004 IF( (.NOT. ERR .AND. .NOT.FIN))THEN
         CALL QLXTOK
         IF( (PREVI .EQ.4))THEN
            IF( (TYPE .EQ.0))THEN
               KLE = TOKEN(1:8)
               PREVI =7
               IF((INLIST))THEN
                  NPRM = MIN(NPRM+1,101)
                  PARM(NPRM) = QLXVAL(KLE,ERR)
               ELSE 
                  NARG = MIN(NARG+1,41)
                  ADR(NARG) = QLXADR(KLE,ERR)
                  DOPEA(NARG) = NDOPES + 1
                  NPRM0 = NPRM - 1
               ENDIF 
               NDOPES = MIN(NDOPES+1,101)
               DOPES(NDOPES) = TYPE + 1 * 256 + (NPRM-NPRM0)*256 * 256
               DOPE(NARG) = DOPE(NARG) + 1
!
            ELSE 
               IF( (TYPE.EQ.1 .OR. TYPE.EQ.2))THEN
                  NPRM = MIN(NPRM+1,101)
                  PARM(NPRM) = JVAL
                  PREVI =7
                  IF((.NOT. INLIST))THEN
                     NARG = MIN(NARG+1,41)
                     ADR(NARG) =get_address_from(PARM(NPRM))
                     DOPEA(NARG) = NDOPES + 1
                     NPRM0 = NPRM - 1
                  ENDIF 
                  NDOPES = MIN(NDOPES+1,101)
                  DOPES(NDOPES) = TYPE + 1 * 256 + (NPRM-NPRM0)*256*256
                  DOPE(NARG) = DOPE(NARG) + 1
!
               ELSE 
                  IF( (TYPE .EQ.3))THEN
                     JLEN = MIN((LEN+KARMOT-1) / KARMOT , 101 - NPRM)
                     IF((.NOT. INLIST))THEN
                        NARG = MIN(NARG+1,41)
                        ADR(NARG) =get_address_from(PARM(NPRM+1))
                        DOPEA(NARG) = NDOPES + 1
                        NPRM0 = NPRM
                     ENDIF 
                     READ(TOKEN,LINEFMT) (PARM(J+NPRM),J=1,JLEN)
! 101                  FORMAT(25 A04)
                     NDOPES = MIN(NDOPES+1,101)
                     DOPES(NDOPES) = TYPE + LEN * 256 + (NPRM-NPRM0+1)*256 *256
                     NPRM = MIN(NPRM+JLEN,101)
!

                     DOPE(NARG) = DOPE(NARG) + JLEN
                     PREVI =7
!
                  ELSE 
                     IF((TYPE.EQ.4 .AND. TOKEN(1:1).EQ.'[' .AND. .NOT. INLIST))THEN
                        INLIST = .TRUE.
                        PREVI =4
                        NARG = MIN(NARG+1,41)
                        ADR(NARG) =get_address_from(PARM(NPRM+1))
                        DOPEA(NARG) = NDOPES + 1
                        NPRM0 = NPRM
                     ELSE 
                        IF((TYPE.EQ.4 .AND. TOKEN(1:1).EQ.')' .AND. NARG.EQ.0))THEN
                           FIN = .TRUE.
                        ELSE 
                           CALL QLXERR(81019,'QLXCALL')
                           ERR = .TRUE.
!
                        ENDIF 
                     ENDIF 
                  ENDIF 
               ENDIF 
            ENDIF 
!
         ELSE 
            IF( (TYPE.EQ.4 .AND. (TOKEN(1:1).EQ.',' .OR. TOKEN(1:1) .EQ.')')))THEN
               FIN = TOKEN(1:1).EQ.')'
               PREVI =4
!
            ELSE 
               IF((TYPE.EQ.4 .AND. TOKEN(1:1).EQ.']' .AND. INLIST))THEN
                  INLIST = .FALSE.
!
               ELSE 
                  CALL QLXERR(81020,'QLXCALL')
                  ERR = .TRUE.
!
               ENDIF 
            ENDIF 
!
         ENDIF 
!
         GOTO 23004
      ENDIF 
      DOPEA(NARG+1) = NDOPES + 1
      IF( (.NOT. ERR))THEN
         LIM1 = LIMITS/100
         LIM2 = MOD(LIMITS,100)
         IF( (NARG.GT.40 .OR. NPRM.GT.100 .OR. NDOPES .GT. 100))THEN ! too many arguments or total number of items too large
            CALL QLXERR(81021,'QLXCALL')
            ERR = .TRUE.
         ELSE 
            IF( (NARG.LT.LIM1 .OR. NARG.GT.LIM2))THEN                ! number of arguments not within specified range for this routine
               CALL QLXERR(81022,'QLXCALL')
               ERR = .TRUE.
            ELSE 
               call set_content_of_location(ICOUNT,1,NARG)   ! set number of arguments
               JUNK=RMTCALL(SUB,ADR)
               call set_content_of_location(ICOUNT,1,0)      ! set number of arguments to zero after call
               CALL QLXFLSH('$')                             ! flush rest of line
            ENDIF 
         ENDIF 
!
      ENDIF 
      RETURN
      END
!
!**FONCTION QLXCHR     RETOURNE UN CARACTERE A LA FOIS D'UNE LIGNE
      CHARACTER(len=1) FUNCTION QLXCHR()  ! get next character from current input stream
      use readlx_internals
      implicit none
!
!
!AUTEUR M.VALIN  RPN  JUIN 1983
!
!OBJET(QLXCHR)
!        RETOURNE UN CARACTERE D'UNE LIGNE DE TEXTE,
!        ET AVANCE LE POINTEUR D'UNE POSITION.
!ARGUMENT
!        QLXCHR    CARACTERE RENVOYE(1 CARACTERE HOLLERITH)
!      S
!

!       COMMON /QLXBUFF/ NC,LAST,INPFILE,EOFL,NERR,SKIPFLG
!       COMMON /QLXBUFF/ CURREC,READREC,TMPFILE
!       INTEGER NC,LAST,INPFILE,NERR,SKIPFLG,CURREC,READREC,TMPFILE
!       LOGICAL EOFL
!       COMMON /QLXBUF2/ INLINE
!       CHARACTER *101 INLINE
!
      CHARACTER(len=8), dimension(0:3), save :: SKIPMSG
      LOGICAL COMMENT
      INTEGER PRTFLAG
      DATA SKIPMSG/'<<    >>','<<SKIP>>','<<SKIP>>','<< ** >>'/
!       DATA NC,LAST/1,0/
!       DATA INPFILE/5/
!       DATA EOFL/.FALSE./
!       DATA INLINE/' '/
!
      IF((NC.LE.LAST))THEN    ! data available in line buffer ?
         QLXCHR=INLINE(NC:NC) ! return current character
         NC=NC+1              ! bump current char pointer
      ELSE                    ! buffer is empty
         IF( (.NOT. EOFL))THEN
1           CONTINUE
            IF((READREC.GT.CURREC))THEN
               READREC=0
            ENDIF 
            IF((READREC.EQ.0))THEN                               ! NOT READING FROM PAST HISTORY
               READ(INPFILE,'(A80)',END=10)INLINE(21:100)        ! read pos 21 and after
               CURREC = CURREC + 1
               WRITE(TMPFILE,'(A80)',REC=CURREC)INLINE(21:100)   ! WRITE LINE TO PAST HISTORY
            ELSE                                                 ! READING FROM PAST HISTORY
               READ(TMPFILE,'(A80)',REC=READREC)INLINE(21:100)
               READREC = READREC + 1                             ! bump record number
            ENDIF 
            INLINE(1:20) = ' '                                   ! set pushback area to blanks
            COMMENT = .FALSE.
            PRTFLAG = SKIPFLG
            IF( (INLINE(21:21).EQ.'C' .OR. INLINE(21:21).EQ.'*' .OR. INLINE(21:21) .EQ.'#'))THEN
               IF( (PRTFLAG.EQ. 0))THEN   ! not skipping
                  COMMENT = .TRUE.
                  PRTFLAG = 3             ! comment marker
               ELSE 
                  COMMENT = .TRUE.
               ENDIF 
            ENDIF 
            WRITE(6,'(1X,A8,1X,A80)')   SKIPMSG(PRTFLAG),INLINE(21:100)  ! print line
            IF( ((INLINE.EQ.' ') .OR. (COMMENT)))THEN                    ! blank line or comment, get next line
               GOTO 1
            ENDIF 
            LAST=100
23014       IF((LAST.GT.21 .AND.INLINE(LAST:LAST).EQ.' '))THEN  ! remove trailing blanks
               LAST=LAST-1
               GOTO 23014
            ENDIF 
            IF( (INLINE(LAST:LAST) .EQ.'_'))THEN
               LAST = LAST-1
            ELSE 
               IF( (INLINE(LAST:LAST) .NE.','))THEN
                  LAST = LAST+1
                  INLINE(LAST:LAST) ='$'  ! END OF LINE marker set if line does not end with ,
               ENDIF 
            ENDIF 
            QLXCHR=INLINE(21:21)     ! return first char in line buffer
            NC=22                    ! bump pointer
         ELSE ! OOPS, hitting end of file
            CALL QLXERR(81008,'QLXCHR')
            CALL ABORT
         ENDIF 
      ENDIF 
      RETURN

10    INLINE = ' END$'  ! store END line if hitting end of file
      QLXCHR=' '        ! return space character
      EOFL=.TRUE.       ! set end of file marker
      LAST=5            ! last char is 5
      NC=2              ! next position is 2
      RETURN
      END
!
      SUBROUTINE QLXDBG ! print line buffer and some associated information
      use readlx_internals
      implicit none
!       COMMON /QLXBUFF/ NC,LAST,INPFILE,EOFL,NERR,SKIPFLG
!       COMMON /QLXBUFF/ CURREC,READREC,TMPFILE
!       INTEGER NC,LAST,INPFILE,NERR,SKIPFLG,CURREC,READREC,TMPFILE
!       LOGICAL EOFL
!       COMMON /QLXBUF2/ INLINE
!       CHARACTER *101 INLINE
      WRITE(6,*) 'NC=',NC,'LAST=',LAST,'INPFILE=',INPFILE
      WRITE(6,'(1X,A101)')INLINE(1:101)
      RETURN
      END
!
!**FUNCTION QLXDTYP  TYPE OF A NUMERICAL DATA ITEM  (TO BE REVISITED BECAUSE NEXT TO POINTLESS)
      FUNCTION QLXDTYP(ITEM)   ! ONLY USED BY QLXADI
      implicit none
      INTEGER QLXDTYP
      INTEGER ITEM
      IF((ABS(ITEM).LE.2147483647))THEN
         QLXDTYP =1                      ! assumed INTEGER if abs value < 2**31 (almost always the case if integer is 32 bits)
      ELSE 
         QLXDTYP =2                      ! REAL
      ENDIF 
      RETURN
      END

!
!**S/P QLXERR     IMPRIME DES MESSAGES D'ERREUR
      SUBROUTINE QLXERR(CODE,MODUL)
      use readlx_internals
      implicit none
      INTEGER CODE
      CHARACTER(len=*) :: MODUL
!
!
!AUTEUR  M.VALIN  RPN  JUIN 1983
!
!OBJET
!        IMPRIME LE NOM DU MODULE DANS LEQUEL UNE ERREUR EST DETECTEE,
!        LE TYPE D'ERREUR, ET LE MESSAGE D'ERREUR APPROPRIE. SI L'ERREUR
!        EST FATALE, IL FAIT UN ABORT.
!ARGUMENTS
!        CODE
!        MODULE    DE TYPE CARACTERE. DESIGNE LE MODULE DANS LEQUEL L'ERREUR  ES
!

!       COMMON /QLXBUFF/ NC,LAST,INPFILE,EOFL,NERR,SKIPFLG
!       COMMON /QLXBUFF/ CURREC,READREC,TMPFILE
!       INTEGER NC,LAST,INPFILE,NERR,SKIPFLG,CURREC,READREC,TMPFILE
!       LOGICAL EOFL
!       COMMON /QLXBUF2/ INLINE
!       CHARACTER *101 INLINE
!*

      INTEGER DESTI,MT,ME,I
      CHARACTER(len=80) :: ERMSG
      CHARACTER(len=7), dimension(9) :: typ
      CHARACTER(len=40), dimension(50) :: MSG

      DATA MSG(  1) /'REPETITION NEGATIF'/
      DATA MSG(  2) /'NB DE FOIS DEJA VU OU NON ENTIER'/
      DATA MSG(  3) /'LA LIMITE EST DEPASSEE'/
      DATA MSG(  4) /'OPERATEUR MAL PLACE'/
      DATA MSG(  5) /'TOKEN MAL PLACE'/
      DATA MSG(  6) /'IL MANQUE LE SIGNE EGAL'/
      DATA MSG(  7) /'DEBORDEMENT DU TAMPON D ENTREE'/
      DATA MSG(  8) /'FIN DU FICHIER DEPASSEE'/
      DATA MSG(  9) /'INDICE NEGATIF, NUL OU NON ENTIER'/
      DATA MSG( 10) /'MANQUE LE CROCHET DROIT'/
      DATA MSG( 11) /'TABLE DES SYMBOLES PLEINE'/
      DATA MSG( 12) /'LIMITE > 99999'/
      DATA MSG( 13) /'MAUVAIS CODE DE TYPE'/
      DATA MSG( 14) /'TOKEN DOUTEUX'/
      DATA MSG( 15) /'CLE MAL UTILISEE'/
      DATA MSG( 16) /'PAS TROUVE LA CLE'/
      DATA MSG( 17) /'INDICE HORS LIMITE OU MAUVAISE CLE'/
      DATA MSG( 18) /'( ATTENDU'/
      DATA MSG( 19) /'OPERANDE DEMANDEE'/
      DATA MSG( 20) /', OU ) ATTENDU'/
      DATA MSG( 21) /'LA PILE D ARGUMENTS DEBORDE'/
      DATA MSG( 22) /'TROP OU PAS ASSEZ D''ARGUMENTS'/
!

      DATA typ( 1) /'INFO   '/
      DATA typ( 2) /'TRIVIAL'/
      DATA typ( 3) /'       '/
      DATA typ( 4) /'       '/
      DATA typ( 5) /'       '/
      DATA typ( 6) /'       '/
      DATA typ( 7) /'       '/
      DATA typ( 8) /'FATAL  '/
      DATA typ( 9) /'SYSTEME'/
!
      MT = CODE / 10000
      NERR = NERR + 1
      ME = MOD(CODE,1000)
      DESTI = MOD(CODE/1000,10)
!
      WRITE(ERMSG,600) ME,MODUL,typ(MT),MSG(ME)
600   FORMAT(' RLX',I3.3,'-',A7,'-',A7,'-',A40)
!
      WRITE(6,*) ERMSG
      WRITE(6,'(1X,A)') INLINE(21:LAST)
      WRITE(6,'(1X,101A1)') (' ',I=1,NC-22),'^'
!
      RETURN
      END
!
!**S/P QLXFLSH     RETIENT  UN SEUL CARACTERE D'UNE LIGNE.
      SUBROUTINE QLXFLSH(ICAR)  ! flush input characters until ICAR found
      implicit none
      CHARACTER(len=1) :: ICAR
!
!AUTEUR M. VALIN  RPN  JUIN 1983
!
!
!OBJET
!        RETOURNE LE PREMIER CARACTERE D'UNE LIGNE DE TEXTE,
!        QUI SOIT EGAL A L'ARGUMENT.
!ARGUMENT
!        ICAR     ENTIER SERVANT D'ARGUMENT D'ENTREE . IL DESIGNE
!                 LE CARACTERE A ETRE RETENU DANS LA LIGNE DE TEXTE.
!*
      CHARACTER(len=1), external :: QLXCHR  ! get netx input char
!
23000 IF((QLXCHR().NE.ICAR))THEN  ! loop until next char is ICAR
         GOTO 23000
      ENDIF 
      RETURN
      END
!
      SUBROUTINE QLXFND(KEY,LOCVAR,LOCCNT,LIMITS,ITYP) ! symbol table lookup
      use readlx_internals
      implicit none
      CHARACTER(len=*), intent(IN) :: KEY   ! symbol to look for
      Integer(kind=8) :: LOCVAR             ! address associated to symbol (0 if not applicable)
      Integer(kind=8) :: LOCCNT             ! count associated with symbol
      INTEGER, intent(OUT) :: LIMITS        ! limits associated with symbol
      INTEGER, intent(OUT) :: ITYP          ! symbol type
!
!        RETROUVE, A PARTIR DE LA CLE IKEY, L'ADRESSE DE IVAR,ICOUNT.
!
      Integer(kind=8), EXTERNAL :: get_address_from
      INTEGER QLXNVAR, QLXUNDF, QLXPRNT
      EXTERNAL QLXNVAR, QLXUNDF, QLXPRNT, LOW2UP
      CHARACTER(len=8), save :: IKEY
      CHARACTER(len=8), save :: CLEF(12)    ! basic keyword table
      INTEGER DUMMY,POS
      SAVE DUMMY
      integer :: i
      DATA CLEF /'END     ','IF      ','ELSE    ','ENDIF   ','WHILE   ','ENDWHILE','ENDDATA ','ENDCASE ','ENDREAD ','@PRINT  ','@DEFINE ','@UNDEF  '/
!
      LOCVAR=0
      LOCCNT=0
      LIMITS=0
      ITYP=-1
      CALL LOW2UP(KEY,IKEY) ! force symbol to upper case
!
      POS = 0
      DO 23000 I = 1,12              ! look first into basic keyword table
         IF( (IKEY.EQ. CLEF(I)))THEN
            POS = I
            GOTO 05
         ENDIF 
23000 CONTINUE 
05    CONTINUE
      GOTO (10,20,30,40,50,60,70,80,90,100,110,120,130) POS+1
10    CONTINUE
      CALL QLXLOOK(LOCVAR,IKEY,LOCCNT,LIMITS,ITYP)  ! not found in basic keyword table
      GOTO 200
20    CONTINUE
      ITYP = 10
      GOTO 200
30    CONTINUE
      ITYP = 3
      GOTO 200
40    CONTINUE
      ITYP = 4
      GOTO 200
50    CONTINUE
      ITYP = 5
      GOTO 200
60    CONTINUE
      ITYP = 6
      GOTO 200
70    CONTINUE
      ITYP = 7
      GOTO 200
80    CONTINUE
      ITYP = 11
      GOTO 200
90    CONTINUE
      ITYP = 12
      GOTO 200
100   CONTINUE
      ITYP = 13
      GOTO 200
110   CONTINUE
      ITYP = 2
      LOCVAR = get_address_from(QLXPRNT)   ! print(QUOI,FORMAT)
      LOCCNT = get_address_from(DUMMY)
      LIMITS = 202
      GOTO 200
120   CONTINUE
      ITYP = 2
      LOCVAR = get_address_from(QLXNVAR)   ! define(KEY,DEFINITION)
      LOCCNT = get_address_from(DUMMY)
      LIMITS = 202
      GOTO 200
130   CONTINUE
      ITYP = 2
      LOCVAR = get_address_from(QLXUNDF)   ! undef(KEY)
      LOCCNT =get_address_from(DUMMY)
      LIMITS = 101
200   CONTINUE
      RETURN
      END
!
      SUBROUTINE QLXIND(IND,ERR)  ! look for possible index, if none found, return 1
      use readlx_internals
      implicit none
!
      INTEGER, intent(OUT) :: IND  ! index value
      LOGICAL, intent(OUT) :: ERR  ! error flag
!       COMMON/QLXTOK1/LEN,TYPE,ZVAL,INEXPR
!       LOGICAL INEXPR
!       INTEGER LEN,TYPE,JVAL
!       REAL ZVAL
!       EQUIVALENCE (ZVAL,JVAL)
! !
! 
!       COMMON/QLXTOK2/TOKEN
!       CHARACTER *80 TOKEN
!
!*
      EXTERNAL QLXSKP
      CHARACTER(len=1) :: QLXSKP
      CHARACTER(len=1) :: IC
      IND=1
      IC=QLXSKP(' ')
!
      IF((IC.EQ.'['))THEN        ! yes, there is indexing [number]
         CALL QLXTOK             ! get next token
         IF((((TYPE.EQ.1) .OR.(TYPE.EQ.0)) .AND. JVAL.GT.0))THEN   ! MUST be a positive number (KEY or integer)
            IND=JVAL
         ELSE 
            CALL QLXERR(21009,'QLXIND')
            ERR=.TRUE.
         ENDIF 
         IF((.NOT.ERR))THEN
            CALL QLXTOK
            IF((TOKEN(1:1).NE.']' .OR. TYPE.NE.4))THEN   ! MUST end with ]
               CALL QLXERR(21010,'QLXIND')
               ERR=.TRUE.
            ENDIF 
         ENDIF 
      ELSE 
         CALL QLXBAK(IC)
      ENDIF 
      RETURN
      END
!
!**S/P QLXINX DECLARATION DES ROUTINES
!
      SUBROUTINE QLXINX(XTERN,KEY,ICOUNT,LIMITS,ITYP)
      implicit none
      EXTERNAL XTERN
      INTEGER ITYP,LIMITS
      Integer ICOUNT
      CHARACTER(len=*) :: KEY
!
      INTEGER IDUM
!
      IF( (ITYP.NE. 2))THEN
         PRINT *,' *** QLXINX ne peut etre utilise pour ityp <> 2'
         CALL QLXERR(81013,'QLXINS')
         STOP
      ENDIF 
      CALL QQLXINS(IDUM,KEY,ICOUNT,LIMITS,ITYP,XTERN)
      RETURN
      END

!**S/P QLXINS DECLARATION DES CLES
!
      SUBROUTINE QLXINS(IVAR,KEY,ICOUNT,LIMITS,ITYP)
      implicit none
      Integer IVAR,ICOUNT
      INTEGER ITYP,LIMITS
      CHARACTER(len=*) :: KEY
!
      EXTERNAL READLX
!
      IF( (ITYP.EQ. 2))THEN
         PRINT *,' *** QLXINX doit etre utilise plutot que QLXINS quand ityp = 2'
         CALL QLXERR(81013,'QLXINS')
         STOP
!           PRINT *,' *** QLXINX doit etre utilise quand ityp = 2',
!                   ' au lieu de QLXINS'
!           CALL QLXERR(10013,'QLXINS')
!           CALL QQLXINS(IVAR,KEY,ICOUNT,LIMITS,ITYP,IVAR)

      ELSE 
         CALL QQLXINS(IVAR,KEY,ICOUNT,LIMITS,ITYP,READLX)
      ENDIF 
      RETURN
      END

!**S/P QQLXINS DECLARATION DES CLES ET DE LEUR TYPE
!
      SUBROUTINE QQLXINS(IVAR,KEY,ICOUNT,LIMITS,ITYP,XTERN)  ! install symbol in tables
      use readlx_internals
      implicit none
      Integer IVAR,ICOUNT
      EXTERNAL XTERN
      INTEGER ITYP,LIMITS
      CHARACTER(len=*) :: KEY
!
!        CONSTRUIT UNE TABLE CONTENANT LA CLE(IKEY), L'ADRESSE DES
!        VALEURS IVAR(MAXIMUM DE 'LIMITS')ET DU NOMBRE DE VALEURS(ICOUNT),
!        LE NOMBRE MAXIMUM DE VALEURS,ET LE TYPE DE SYMBOLES.
!
!*
!
!     TABLES STATIQUES CONTENANT LES CLES, LEURS ADRESSES, ET LES LIMITES
!
      CHARACTER(len=8) :: IKEY
      integer :: IPNT
      integer, external :: get_address_from

!       INTEGER ITAB(3:3,256),NENTRY
!       Integer(kind=8), dimension(2,256) :: IPTADR
! 
!       CHARACTER *8 NAMES(256)
!       COMMON /qqq_nrdlx/ NAMES, ITAB, NENTRY
!       COMMON /qqq_nrdlx2/ IPTADR
!       
!       DATA ITAB /256 * 0/
!       DATA IPTADR /256 * 0,256 * 0/
!       DATA NAMES /256 * ' '/
!       DATA NENTRY /0/
!
!     TROUVER LA CLE
!
      CALL LOW2UP(KEY,IKEY)  ! force key to upper case
      IPNT=NENTRY
23000 IF((IPNT.GT. 0 .AND. IKEY.NE.NAMES(IPNT)))THEN
         IPNT = IPNT - 1
         GOTO 23000
      ENDIF 
      IF((IPNT.EQ.0))THEN
         NENTRY=NENTRY+1
         IPNT=NENTRY
      ENDIF 
      IF((IPNT.EQ.256))THEN
         CALL QLXERR(10011,'QLXINS')
      ENDIF 
      IF((LIMITS.LT.0 .OR. LIMITS.GT.99999))THEN
         CALL QLXERR(20012,'QLXINS')
         RETURN
      ENDIF 
      IF((ITYP.LT.0 .OR.ITYP.GT.13))THEN
         CALL QLXERR(20013,'QLXINS')
         RETURN
      ENDIF 
      ICOUNT=0
      NAMES(IPNT)=IKEY
      IF( (ITYP.EQ. 2))THEN
         IPTADR(1,IPNT)=get_address_from(XTERN)
      ELSE 
         IPTADR(1,IPNT)=get_address_from(IVAR)
      ENDIF 
      ITAB(3,IPNT)=IOR(LIMITS,ishft(ITYP,24))
      IPTADR(2,IPNT)=get_address_from(ICOUNT)
      RETURN
      END
!
      SUBROUTINE QLXLOOK(IVAR,KEY,ICOUNT,LIMITS,ITYP)  ! lookup routine for user defined symbols
      use readlx_internals
      implicit none
      Integer(kind=8), intent(OUT) :: ivar    ! address
      CHARACTER(len=*), intent(IN) :: KEY     ! symbol
      Integer(kind=8), intent(OUT) :: icount  ! count
      INTEGER, intent(OUT) :: LIMITS          ! limits for number of values/arguments (max + 100 * min)
      INTEGER, intent(OUT) :: ITYP            ! symbol type
!       INTEGER ITAB(3:3,256),NENTRY
!       Integer*8 IPTADR(2,256)
!       CHARACTER *8 NAMES(256)
!       COMMON /qqq_nrdlx/ NAMES, ITAB, NENTRY
!       COMMON /qqq_nrdlx2/ IPTADR
      character(len=8) :: ikey
      integer :: IPNT
!
!     TROUVER LA CLE
!
      CALL LOW2UP(KEY,IKEY)   ! force key to upper case
      IPNT=NENTRY
23012 IF((IPNT.GT. 0 .AND. IKEY.NE.NAMES(IPNT)))THEN
         IPNT = IPNT - 1
         GOTO 23012
      ENDIF 
      IF((IPNT .EQ. 0))THEN  ! NOT FOUND
         ITYP = -1
         IVAR = 0
         ICOUNT = 0
         LIMITS = 0
         RETURN
      ENDIF 
!
!     DECORTIQUER LES PARAMETRES DE LA CLE
!
      IVAR=IPTADR(1,IPNT)
      ICOUNT=IPTADR(2,IPNT)
      LIMITS=IAND(ITAB(3,IPNT),ishft(-1,-(32-(24))))
      ITYP=ishft(ITAB(3,IPNT),-(24))
      RETURN
      END
!
      subroutine QLXUDF(IVAR,KEY)  ! undefine a key (symbol)
      use readlx_internals
      implicit none
      Integer(kind=8), intent(IN) :: ivar    ! address
      CHARACTER(len=*), intent(IN) :: KEY     ! symbol
      integer :: I, IPNT
      character(len=8) :: ikey
!
!     TROUVER LA CLE
!
      IKEY = KEY
      IPNT=NENTRY
23016 IF((IPNT.GT. 0 .AND. IKEY.NE.NAMES(IPNT)))THEN
         IPNT = IPNT - 1
         GOTO 23016
      ENDIF 
      IF((IPNT .EQ. 0))THEN  ! NOT FOUND
         RETURN
      ENDIF 
      DO 23020 I=IPNT, NENTRY-1        ! shift tables down by one position to get rid of symbol
         IPTADR(1,I) = IPTADR(1,I+1)
         ITAB(3,I) = ITAB(3,I+1)
         IPTADR(2,I) = IPTADR(2,I+1)
         NAMES(I)  = NAMES(I+1)
23020 CONTINUE 
      NENTRY = NENTRY - 1
      RETURN
      END
!
      subroutine QLXDTB      ! DUMP symbol table
      use readlx_internals
      implicit none
      integer :: I
      PRINT *,' NAMES, LOCVAR, TYPE/LIMITS, LOCCOUNT'
      DO 23022 I=1,NENTRY
         PRINT 101, NAMES(I),IPTADR(1,I),ITAB(3,I),IPTADR(2,I)
23022 CONTINUE 
101   FORMAT (2X,A8,3Z22)
      RETURN
      END

!
!**FONCTION QLXNUM    RECONSTITUER UN NOMBRE ENTIER, REEL OU OCTAL (ONLY USED BY QLXTOK)
      FUNCTION QLXNUM(IB,LENG)
      implicit none
      INTEGER QLXNUM
      CHARACTER(len=*), intent(INOUT) :: IB         ! input token (my get updated)
      INTEGER, intent(INOUT)          :: LENG       ! length of token (will be updated and may get clipped)
!
!
!AUTEUR     M.VALIN    RPN    JUIN 1983
!
!OBJET(QLXNUM)
!        A PARTIR D'UN TOKEN COMMENCANT PAR UN CHIFFRE, RECONSTITUER
!        LE NOMBRE. INDIQUER S'IL EST ENTIER OU REEL.
!ARGUMENT
!        QLXNUM    RETOURNE   -10   POUR UN NOMBRE REEL
!        (S)                   -9   POUR UN NOMBRE ENTIER
!                              -3   ERREUR
!                              5    BAD NUMBER, > 21 digits, or other error
!                              1    integer
!                              2    float
!                              6    octal
!
!        IB(*)     IB(1) EST LE PREMIER CHIFFRE DU NOMBRE.
!       (E/S)        LA TABLE IB CONTIENT LE NOMBRE.
!
!        LENG      NOMBRE DE CARACTERES DANS LE NOMBRE(ENTIER OU REEL)
!        (S)
!*
      INTEGER ILX, J
      EXTERNAL QLXCHR
      CHARACTER(len=1) :: I, CTMP, QLXCHR
!
      IF((IB(1:1).EQ.'.'))THEN
         ILX=1  ! decimal point present
      ELSE 
         ILX=0  ! no decimal point
      ENDIF 
      I=QLXCHR()
!
23002 IF((I.GE.'0' .AND. I.LE.'9' ))THEN  ! while numeric digit
         LENG=MIN(21,LENG+1)
         IB(LENG:LENG)=I
         I=QLXCHR()
         GOTO 23002
      ENDIF

      IF((I.EQ.'.' .AND. IB(1:1).NE.'.'))THEN ! decimal number, period not at beginning
         ILX=1                                ! decimal point present
         LENG=MIN(21,LENG+1)
         IB(LENG:LENG)=I
         I=QLXCHR()
23006    IF((I.GE.'0' .AND. I.LE.'9'))THEN  ! while numeric digit
            LENG=MIN(21,LENG+1)
            IB(LENG:LENG)=I
            I=QLXCHR()
            GOTO 23006
         ENDIF 
      ENDIF ! decimal number

      IF((I.EQ.'E' ))THEN    ! exponent detected
         IF((ILX.EQ.0))THEN  ! for decimal point if none present
            LENG=MIN(21,LENG+1)
            IB(LENG:LENG)='.'
         ENDIF 
         ILX=1               ! decimal point present
         LENG=MIN(21,LENG+1)
         IB(LENG:LENG)=I
         I=QLXCHR()
         IF(( (I.GE.'0' .AND. I.LE.'9') .OR. (I.EQ.'+') .OR. (I.EQ.'-') ))THEN ! collect exponent
6           LENG=MIN(21,LENG+1)
            IB(LENG:LENG)=I
            I=QLXCHR()
            IF((I.GE.'0' .AND. I.LE.'9'))THEN
               GOTO 6
            ENDIF 
         ENDIF 
      ENDIF

      IF((LENG.GE.21))THEN   ! abusively large number
         QLXNUM=5
      ELSE 
         IF((ILX.EQ.0))THEN   ! no decimal point
            IF((I.NE.'B'))THEN
               QLXNUM=1
            ELSE 
               QLXNUM=6
               I=QLXCHR()
               DO 23022  J=LENG,1,-1
                  IF((IB(J:J).GT.'7'))THEN
                     QLXNUM=5
                  ENDIF 
                  CTMP = IB(J:J)
                  IB(20-LENG+J:20-LENG+J)=CTMP
23022          CONTINUE 
               DO 23026  J=1,20-LENG
                  IB(J:J)='0'
23026          CONTINUE 
               LENG=20
            ENDIF 
         ELSE               ! decimal point
            IF((LENG.GT.1))THEN
               IF((IB(LENG:LENG).EQ.'.'))THEN
                  QLXNUM=2
               ELSE 
                  IF((IB(LENG:LENG).GE.'0' .AND. IB(LENG:LENG).LE.'9'))THEN
                     QLXNUM=2
                  ELSE 
                     QLXNUM=5
                  ENDIF 
               ENDIF 
            ELSE 
               QLXNUM=5
            ENDIF 
         ENDIF 
      ENDIF 
      CALL QLXBAK(I)
!
      RETURN
      END
!
      SUBROUTINE QLXNVAR(KEY,NW)
      use readlx_internals
      implicit none
      INTEGER NW

      INTEGER KEY(*)
      INTEGER, EXTERNAL :: ARGDIMS
      INTEGER SC(1024),NSC,J,ITYP,LIMITS
      integer(kind=8) :: IVAR,ICOUNT

!       CHARACTER * 20 LINEFMT
!       INTEGER KARMOT
!       COMMON /QLXFMT/ LINEFMT
!       COMMON /QLXFMT2/ KARMOT
      SAVE SC, NSC
      INTEGER DUMMY
      CHARACTER *8 IKEY
      SAVE DUMMY
      DATA NSC /1/
      DATA DUMMY /0/
      WRITE(IKEY,LINEFMT) (KEY(J),J=1,ARGDIMS(1))
! 101   FORMAT(2 A04)
      CALL QLXLOOK(IVAR,IKEY,ICOUNT,LIMITS,ITYP)
      IF((ITYP.NE.-1))THEN
         RETURN
      ENDIF 
      IF((NSC+NW .GT.1024+1))THEN
         CALL QLXERR(21011,'DEFINE')
         RETURN
      ENDIF 
      CALL QLXINS(SC(NSC),IKEY,DUMMY,NW,1)
      NSC = NSC + NW
      RETURN
      END
#if defined(WITH_EXPRESSIONS)
!**S/P QLXOPR APPLIQUER UN OPERATEUR NUMERIQUE OU LOGIQUE
      SUBROUTINE QLXOPR(TOKENS,NTOKEN,TOKTYPE,OPRTR,ERR)   ! THIS IS BROKEN ON 64 BIT MACHINES
      implicit none
      INTEGER NTOKEN,OPRTR,TOKENS(NTOKEN),TOKTYPE(NTOKEN)
      LOGICAL ERR
!      EXTERNAL QLXMAD
!      INTEGER  QLXMAD
      Integer*8 get_address_from
      EXTERNAL get_address_from


      INTEGER IZ1, IZ2, IR1, MINOPER
      REAL   Z1,  Z2,  R1
      EQUIVALENCE (IZ1,Z1),(IZ2,Z2),(IR1,R1)
      LOGICAL REALOP
      integer :: TOK
      POINTER (PTOK,TOK(*))
      IF((ERR))THEN
         RETURN
      ENDIF 
      IF((OPRTR.EQ.4 .OR. OPRTR.EQ.17))THEN
         MINOPER = 1
      ELSE 
         MINOPER = 2
      ENDIF 
      IF((NTOKEN.LT.MINOPER))THEN
         ERR = .TRUE.
         RETURN
      ENDIF 
      IF((TOKTYPE(NTOKEN).GT.0))THEN
         call get_content_of_location(TOKENS(NTOKEN),1,TOKENS(NTOKEN))
         TOKTYPE(NTOKEN) = 0
      ENDIF 
      IF((OPRTR.NE.2 .AND. OPRTR.NE.17   .AND. OPRTR.NE.21 .AND. OPRTR.NE.4))THEN
         IF((TOKTYPE(NTOKEN-1).GT.0))THEN
            call get_content_of_location(TOKENS(NTOKEN-1),1,TOKENS(NTOKEN-1))
            TOKTYPE(NTOKEN-1) = 0
         ENDIF 
      ENDIF 
      REALOP = ABS(TOKENS(NTOKEN)).GT.2147483647
      IZ1 = TOKENS(NTOKEN)
      IF((OPRTR.NE.2 .AND. OPRTR.NE.17 .AND. OPRTR.NE.4))THEN
         REALOP = REALOP .OR. ABS(TOKENS(NTOKEN-1)).GT.2147483647
         IZ2 = TOKENS(NTOKEN-1)
         IF((REALOP))THEN
            IF((ABS(IZ1).LE.2147483647))THEN
               Z1 = TOKENS(NTOKEN)
            ENDIF 
            IF((ABS(IZ2).LE.2147483647))THEN
               Z2 = TOKENS(NTOKEN-1)
            ENDIF 
         ENDIF 
      ENDIF 
      IR1 = 0
      GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)OPRTR
1     ERR = .TRUE.
      RETURN
2     CONTINUE
      IF((TOKENS(NTOKEN).LE.0 .OR. TOKTYPE(NTOKEN-1).LE.0 .OR.REALOP))THEN
         ERR = .TRUE.
         RETURN
      ENDIF 
      IF((TOKENS(NTOKEN).GE.TOKTYPE(NTOKEN-1)))THEN
         ERR = .TRUE.
         RETURN

!temporaire      TOKENS(NTOKEN-1)=QLXMAD(TOKENS(NTOKEN-1),TOKENS(NTOKEN))
      ENDIF 
      PTOK = get_address_from(TOKENS(NTOKEN-1))
      TOKENS(NTOKEN-1) = TOK(TOKENS(NTOKEN))
      NTOKEN = NTOKEN - 1
      TOKTYPE(NTOKEN) = 1
      RETURN
3     RETURN
4     CONTINUE
      IF((REALOP))THEN
         R1 = -Z1
      ELSE 
         IR1 = -IZ1
      ENDIF 
      GOTO 1000
5     CONTINUE
      IF((REALOP))THEN
         R1 = Z2**Z1
      ELSE 
         IR1 = IZ2**IZ1
      ENDIF 
      GOTO 1000
6     CONTINUE
      IF((REALOP))THEN
         R1 = Z2*Z1
      ELSE 
         IR1 = IZ2*IZ1
      ENDIF 
      GOTO 1000
7     CONTINUE
      IF((REALOP))THEN
         R1 = Z2/Z1
      ELSE 
         IR1 = IZ2/IZ1
      ENDIF 
      GOTO 1000
8     CONTINUE
      IF((REALOP))THEN
         R1 = Z2+Z1
      ELSE 
         IR1 = IZ2+IZ1
      ENDIF 
      GOTO 1000
9     CONTINUE
      IF((REALOP))THEN
         R1 = Z2-Z1
      ELSE 
         IR1 = IZ2-IZ1
      ENDIF 
      GOTO 1000
10    CONTINUE
      IF((REALOP))THEN
         IF((Z2.LT.Z1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ELSE 
         IF((IZ2.LT.IZ1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ENDIF 
      GOTO 1000
11    CONTINUE
      IF((REALOP))THEN
         IF((Z2.GT.Z1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ELSE 
         IF((IZ2.GT.IZ1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ENDIF 
      GOTO 1000
12    CONTINUE
      IF((REALOP))THEN
         IF((Z2.EQ.Z1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ELSE 
         IF((IZ2.EQ.IZ1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ENDIF 
      GOTO 1000
13    CONTINUE
      IF((REALOP))THEN
         IF((Z2.LE.Z1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ELSE 
         IF((IZ2.LE.IZ1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ENDIF 
      GOTO 1000
14    CONTINUE
      IF((REALOP))THEN
         IF((Z2.GE.Z1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ELSE 
         IF((IZ2.GE.IZ1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ENDIF 
      GOTO 1000
15    CONTINUE
      IF((REALOP))THEN
         IF((Z2.NE.Z1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ELSE 
         IF((IZ2.NE.IZ1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ENDIF 
      GOTO 1000
16    CONTINUE
      IF((REALOP))THEN
         IF((Z2.NE.Z1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ELSE 
         IF((IZ2.NE.IZ1))THEN
            IR1 =ishft(-1,32-(32))
         ENDIF 
      ENDIF 
      GOTO 1000
17    CONTINUE
      IF((REALOP))THEN
         ERR = .TRUE.
      ELSE 
         IR1 =NOT(IZ1)
      ENDIF 
      GOTO 1000
18    CONTINUE
      IF((REALOP))THEN
         ERR = .TRUE.
      ELSE 
         IR1 = IAND(IZ2,IZ1)
      ENDIF 
      GOTO 1000
19    CONTINUE
      IF((REALOP))THEN
         ERR = .TRUE.
      ELSE 
         IR1 = IOR(IZ2,IZ1)
      ENDIF 
      GOTO 1000
20    CONTINUE
      IF((REALOP))THEN
         ERR = .TRUE.
      ELSE 
         IR1 = IEOR(IZ2,IZ1)
      ENDIF 
      GOTO 1000
21    CONTINUE
      IF((TOKTYPE(NTOKEN-1).LE.0))THEN
         ERR = .TRUE.
         RETURN
      ENDIF 
      call set_content_of_location(TOKENS(NTOKEN-1),1,TOKENS(NTOKEN))
      NTOKEN = NTOKEN - 1
      RETURN
1000  NTOKEN = NTOKEN + 1 - MINOPER
      TOKENS(NTOKEN) = IR1
      TOKTYPE(NTOKEN) = 0
      RETURN
      END
#endif
!**S/P QLXOPT  -  PASSAGE D'OPTIONS A READLX
!
      SUBROUTINE QLXOPT(OPTION,VAL)  ! only one option recognized : CARMOT , number of chars in a "word" (integer)
      use readlx_internals
      implicit none
      CHARACTER(len=*) OPTION
      INTEGER VAL
!

!       CHARACTER * 20 LINEFMT
!       INTEGER KARMOT
!       COMMON /QLXFMT/ LINEFMT
!       COMMON /QLXFMT2/ KARMOT
!

      IF( (OPTION(1:6).EQ. 'CARMOT'))THEN
         KARMOT = VAL
         WRITE(LINEFMT,'(A,I2,A)') '(25 A',KARMOT,')'
      ELSE 
         WRITE(6,*) ' *** ERREUR QLXOPT, OPTION (',OPTION,') INCONNUE'
      ENDIF 
      RETURN
      END
#if defined(WITH_EXPRESSIONS)
!**FONCTION  QLXPRI EVALUER LA PRIORITE D'UN OPERATEUR
      INTEGER FUNCTION QLXPRI_RL(OPR, LEFTPRI)  ! return operator right or left priority and index in list ( index + 100 * priority)
      implicit none
      CHARACTER(len=*), intent(IN) :: OPR
      LOGICAL, intent(IN) ::  LEFTPRI   ! if true return left priority, if false return right priority

      INTEGER :: I
      CHARACTER(len=4) :: OPRTR
      integer, PARAMETER :: MAXOPER=23
      CHARACTER(len=4), save, dimension(MAXOPER) ::  LISTE
      INTEGER, save, dimension (MAXOPER):: PRI
      ! operator list, order MUST be same as PRI table
      DATA LISTE/   ')   ' ,   ']   ' ,   'U+  ' ,   'U-  ' ,   '**  ' ,   '*   ' ,        &
                    '/   ' ,   '+   ' ,   '-   ' ,   '<   ' ,   '>   ' ,   '==  ' ,        &
                    '<=  ' ,   '>=  ' ,   '<>  ' ,   '><  ' ,   'NOT ' ,   'AND ' ,        &
                    'OR  ' ,   'XOR ' ,   ':=  ' ,   '(   ' ,   '[   ' /
      ! operator priority table : right priority = pri, left priority = pri -mod(pri,2)
      DATA PRI  /   150    ,   150    ,   101    ,   101    ,   91     ,   81     ,        &
                    81     ,   71     ,   71     ,   61     ,   61     ,   61     ,        &
                    61     ,   61     ,   61     ,   61     ,   51     ,   41     ,        &
                    41     ,   4      ,   10     ,   1      ,   1      /
      OPRTR = OPR
      DO 23000 I = 1,MAXOPER
         IF((OPRTR.EQ.LISTE(I)))THEN   ! operator found at position I
            IF((LEFTPRI))THEN
               QLXPRI_RL = I + PRI(I)*100
            ELSE 
               QLXPRI_RL = I + (PRI(I)-MOD(PRI(I),2))*100
            ENDIF 
            RETURN
         ENDIF 
23000 CONTINUE 
      QLXPRI_RL = 0 ! OOPS NOT FOUND
      RETURN
      END

      INTEGER FUNCTION QLXPRI(OPR)  ! return operator right priority and index in list ( index + 100 * priority)
      implicit none
      CHARACTER(len=*), intent(IN) :: OPR
      integer, external :: QLXPRI_RL
      QLXPRI = QLXPRI_RL(OPR, .false.)  ! get right priority
      return
      end

      INTEGER FUNCTION QLXPRIL(OPR)  ! return operator left priority and index in list ( index + 100 * priority)
      implicit none
      CHARACTER(len=*), intent(IN) :: OPR
      integer, external :: QLXPRI_RL
      QLXPRIL = QLXPRI_RL(OPR, .true.)  ! get left priority
      return
      end
#endif
      SUBROUTINE QLXPRNT(QUOI,COMMENT)
      use readlx_internals
      implicit none
      INTEGER QUOI(*), COMMENT(*)
      CHARACTER(len=120) :: FMT
      INTEGER, external :: ARGDIMS
      integer :: I, L1, L2

!       CHARACTER * 20 LINEFMT
!       INTEGER KARMOT
!       COMMON /QLXFMT/ LINEFMT
!       COMMON /QLXFMT2/ KARMOT
      L1 = ARGDIMS(1)
      L2 = MIN(120/KARMOT,ARGDIMS(2))
      IF((L1.LT.1 .OR. L2.LT.1))THEN
         RETURN
      ENDIF 
      WRITE(FMT,LINEFMT)(COMMENT(I),I=1,L2)
! 100   FORMAT(20 A04)
      WRITE(6,FMT)(QUOI(I),I=1,L1)
      RETURN
      END

#if defined(WITH_EXPRESSIONS)
!**S/P QLXRPN CONVERSION A NOTATION POSTFIXE
      SUBROUTINE QLXRPN(TOK,TOKENS,MAXTKNS,NTOKEN,TOKTYPE,PILEOP,MAXOPS,NOPER,ERR)   ! THIS IS BROKEN ON 64 BIT MACHINES
      implicit none
      CHARACTER(len=*) TOK
      INTEGER MAXTKNS,NTOKEN,MAXOPS,NOPER
      INTEGER TOKENS(MAXTKNS), TOKTYPE(MAXTKNS)
      EXTERNAL QLXPRI, QLXPRIL
      INTEGER  QLXPRI, QLXPRIL
      LOGICAL ERR
      CHARACTER(len=4) :: TOKEN
      CHARACTER(len=4) ::  PILEOP(MAXOPS)
      IF((ERR))THEN
         RETURN
      ENDIF 
      TOKEN = TOK
      IF((TOKEN.EQ.'(' .OR. TOKEN.EQ.'['))THEN
         NOPER = MIN(NOPER+1 , MAXOPS)
         PILEOP(NOPER) = TOKEN
      ELSE 
         IF((TOKEN.EQ.')'))THEN
23006       IF((PILEOP(NOPER) .NE.'(' .AND.   PILEOP(NOPER) .NE.'[' .AND.   PILEOP(NOPER) .NE.'$'))THEN
               CALL QLXOPR(TOKENS,NTOKEN,TOKTYPE,MOD(QLXPRI(PILEOP(NOPER)),100),ERR)
               NOPER = NOPER - 1
               GOTO 23006
            ENDIF 
            IF((PILEOP(NOPER).EQ.'('))THEN
               NOPER = NOPER-1
            ELSE 
               ERR = .TRUE.
            ENDIF 
         ELSE 
            IF((TOKEN.EQ.']'))THEN
23012          IF((PILEOP(NOPER) .NE.'(' .AND.   PILEOP(NOPER) .NE.'['  .AND.   PILEOP(NOPER) .NE.'$'))THEN
                  CALL QLXOPR(TOKENS,NTOKEN,TOKTYPE,MOD(QLXPRI(PILEOP(NOPER)),100),ERR)
                  NOPER = NOPER - 1
                  GOTO 23012
               ENDIF 
               IF((PILEOP(NOPER).EQ.'['))THEN
                  CALL QLXOPR(TOKENS,NTOKEN,TOKTYPE,MOD(QLXPRI(']'),100),ERR)
                  NOPER = NOPER-1
               ELSE 
                  ERR = .TRUE.
               ENDIF 
            ELSE 
               IF((TOKEN.EQ.'$'))THEN
23018             IF((PILEOP(NOPER) .NE.'(' .AND.   PILEOP(NOPER) .NE.'['  .AND.   PILEOP(NOPER) .NE.'$'))THEN
                     CALL QLXOPR(TOKENS,NTOKEN,TOKTYPE,MOD(QLXPRI(PILEOP(NOPER)),100),ERR)
                     NOPER = NOPER - 1
                     GOTO 23018
                  ENDIF 
                  IF((PILEOP(NOPER).EQ.'$'))THEN
                     NOPER = NOPER-1
                  ELSE 
                     ERR = .TRUE.
                  ENDIF 
               ELSE 
23022             IF((QLXPRIL(PILEOP(NOPER)).GT.QLXPRI(TOKEN)))THEN
                     CALL QLXOPR(TOKENS,NTOKEN,TOKTYPE,MOD(QLXPRI(PILEOP(NOPER)),100),ERR)
                     NOPER = NOPER -1
                     GOTO 23022
                  ENDIF 
                  NOPER = MIN(NOPER+1 , MAXOPS)
                  PILEOP(NOPER) = TOKEN
               ENDIF 
            ENDIF 
         ENDIF 
      ENDIF 
      RETURN
      END
#endif
!**FONCTION QLXSKP     RETOURNE UN CARACTERE AUTRE QUE ICAR
      FUNCTION QLXSKP(ICAR)
      implicit none
      CHARACTER(len=1) :: QLXSKP
      CHARACTER(len=1) :: ICAR
!
!
!AUTEUR M.VALIN  RPN  JUIN 1983
!
!OBJET
!        RETOURNE LE PREMIER CARACTERE D'UNE LIGNE DE TEXTE,
!        QUI NE SOIT PAS EGAL A L'ARGUMENT.
!ARGUMENT
!        ICAR      ENTIER SERVANT D'ARGUMENT D'ENTREE.IL DESIGNE
!                  LE CARACTERE A ETRE IGNORE DANS LA LIGNE DE TEXTE.
!*

      EXTERNAL QLXCHR
      CHARACTER(len=1) :: CTMP, QLXCHR
!
23000 IF(.TRUE.)THEN
         CTMP = QLXCHR()
         IF(.NOT.(CTMP.NE. ICAR))THEN
            GOTO 23000
         ENDIF 
      ENDIF 
      QLXSKP = CTMP
!
      RETURN
      END
!
!**S/P QLXTOK
      SUBROUTINE QLXTOK  ! get next token, if key or numeric item, JVAL contains value qhen subroutine returns
      use readlx_internals
      implicit none
!
!
!AUTEUR   M.VALIN   RPN   JUIN 1983
!
!OBJET(QLXTOK)
!        DECOMPOSE UNE LIGNE DE TEXTE EN TOKENS DE DIFFERENTS
!        TYPES,IDENTIFIE LA LONGUEUR DU TOKEN ET SON TYPE.
!ARGUMENTS
!        TOKEN
!        (S)
!
!        LEN       NOMBRE DE CARACTERE DANS UN TOKEN
!        (S)
!
!        TYPE      TYPE DU TOKEN(CLE ALPHANUMERIQUE,NOMBRE
!        (S)       ENTIER OU REEL,CHAINE DE CARACTERE OU SYMBOLE).
!
!        JVAL,ZVAL LES VALEURS D'UN NOMBRE ENTIER OU REEL,
!        (S)       CONTENU DANS UN TOKEN.
!

      INTEGER ISGN,ITYP
!       COMMON/QLXTOK1/LEN,TYPE,ZVAL,INEXPR
!       LOGICAL INEXPR
!       INTEGER LEN,TYPE,JVAL
!       REAL ZVAL
!       EQUIVALENCE (ZVAL,JVAL)
! !
! 
!       COMMON/QLXTOK2/TOKEN
!       CHARACTER *80 TOKEN
! !
! 
!       CHARACTER * 20 LINEFMT
!       INTEGER KARMOT
!       COMMON /QLXFMT/ LINEFMT
!       COMMON /QLXFMT2/ KARMOT
!*

      Integer(kind=8) :: LOCVAR,LOCCNT
      EXTERNAL QLXCHR, QLXNUM
      CHARACTER(len=1) :: IC, QLXCHR
      INTEGER  QLXNUM
      integer :: LENG, LIMITS
!       IVAL = -1
      TOKEN = ' '
!

23000 IF(.TRUE.)THEN
         IC = QLXCHR()
         IF(.NOT.(IC.NE.' '))THEN
            GOTO 23000
         ENDIF 
      ENDIF 
      LENG=1
      TOKEN(1:1)=IC
      IF(((IC.GE.'A'.AND.IC.LE.'Z').OR.IC.EQ.'@'.OR.IC.EQ.'_'   .OR. (IC.GE. 'a' .AND. IC.LE. 'z')))THEN
         IC=QLXCHR()
23005    IF(((IC.GE.'A' .AND.IC .LE.'Z').OR.   (IC.GE.'0' .AND. IC.LE.'9')   .OR. (IC.GE. 'a' .AND. IC.LE. 'z')))THEN
            LENG=MIN(81,LENG+1)
            TOKEN(LENG:LENG)=IC
            IC=QLXCHR()
            GOTO 23005
         ENDIF 
         IF((LENG.GT.8))THEN
            TYPE=3             ! alphanumeric token , length >  8, STRING
         ELSE 
            TYPE=0             ! alphanumeric token , length <= 8
         ENDIF 
         CALL QLXBAK(IC)

!
      ELSE 
         IF((IC.EQ.'''' .OR. IC.EQ.'"'))THEN
            LENG=0
23011       IF(.TRUE.)THEN
               LENG=MIN(80,LENG+1)
               TOKEN(LENG:LENG)=QLXCHR()
               IF(.NOT.(TOKEN(LENG:LENG).EQ. IC))THEN
                  GOTO 23011
               ENDIF 
            ENDIF 
            TOKEN(LENG:LENG) = ' '
            LENG = LENG -1
            IF( (IC .EQ.'"'))THEN
               LENG = MIN(LENG,KARMOT)
            ENDIF 
            TYPE=3             ! STRING
!
         ELSE 
            IF(((IC.GE.'0' .AND. IC.LE.'9')   .OR.(IC.EQ.'.')))THEN !   number
               TYPE=QLXNUM(TOKEN,LENG)   ! 1, 2, 5, 6 from qlxnum
               ISGN=1
!
            ELSE 
               IF(((IC.EQ.'+' .OR. IC.EQ.'-').AND.(.NOT.INEXPR) ))THEN  ! sign detected
                  IF((IC.EQ.'+'))THEN
                     ISGN=1
                  ELSE 
                     ISGN=-1
                  ENDIF 
                  IC=QLXCHR()
                  IF(((IC.GE.'0' .AND. IC.LE.'9').OR. IC.EQ.'.'))THEN
                     TOKEN(1:1)=IC
                     TYPE=QLXNUM(TOKEN,LENG)   ! 1, 2, 5, 6 from qlxnum
                  ELSE 
                     CALL QLXBAK(IC)
                     TYPE=4
                  ENDIF 
               ELSE 
                  IF((IC.EQ.'*'))THEN
                     TYPE =4
                     IC=QLXCHR()
                     IF((IC.EQ.'*'))THEN
                        LENG = 2
                        TOKEN = '**'
                     ELSE 
                        CALL QLXBAK(IC)
                     ENDIF 
                  ELSE 
                     IF((IC.EQ.'<' .OR. IC.EQ.'>' .OR. IC.EQ.'=' .OR. IC.EQ.':'))THEN
                        TYPE =4
                        IC=QLXCHR()
                        IF((IC.EQ.'<' .OR. IC.EQ.'>' .OR. IC.EQ.'='))THEN
                           LENG = 2
                           TOKEN(2:2) = IC
                        ELSE 
                           CALL QLXBAK(IC)
                        ENDIF 
                     ELSE 
                        TYPE=4

!
                     ENDIF 
                  ENDIF 
               ENDIF 
            ENDIF 
         ENDIF 
      ENDIF 
      IF(((LENG.GT.80) .OR. (TYPE.EQ.5)))THEN
         TOKEN = 'SCRAP'
         TYPE=5
         CALL QLXERR(21014,'QLXTOK')
      ENDIF 
      IF((TYPE.EQ.1))THEN           ! decode integer
         READ(TOKEN,'(I20)')JVAL
         JVAL=SIGN(JVAL,ISGN)
      ELSE 
         IF((TYPE.EQ.2))THEN        ! decode float
            READ(TOKEN,'(G20.3)')ZVAL
            ZVAL=SIGN(ZVAL,FLOAT(ISGN))
         ELSE 
            IF((TYPE.EQ.6))THEN     ! decode octal
               READ(TOKEN,'(O20)')JVAL
               TYPE=1
               JVAL=SIGN(JVAL,ISGN)
            ENDIF 
         ENDIF 
      ENDIF 
      IF((TYPE.EQ.0))THEN               ! possible keyword
         CALL QLXFND(TOKEN(1:8),LOCVAR,LOCCNT,LIMITS,ITYP)
         IF( (ITYP .EQ. -1))THEN
            TYPE =3                     ! not found, it is a string
            LENG = MIN(LENG,KARMOT)
         ELSE 
            IF( ((ITYP .EQ. 0) .OR. (ITYP .EQ. 1)))THEN     ! integer or key, get 4 byte value
               call get_content_of_location(LOCVAR,1,JVAL)
            ELSE 
               JVAL = -1
            ENDIF 
         ENDIF 
      ENDIF 
      LEN=LENG
!

      RETURN
      END
!
      SUBROUTINE QLXUNDF(IKEY)
      use readlx_internals
      implicit none
      INTEGER IKEY(*)
      CHARACTER *8 CKEY
      INTEGER, external :: ARGDIMS
      integer :: I
      integer(kind=8) :: SCRAP
!       CHARACTER * 20 LINEFMT
!       INTEGER KARMOT
!       COMMON /QLXFMT/ LINEFMT
!       COMMON /QLXFMT2/ KARMOT
!
!      WRITE(CKEY,LINFMT)(IKEY(I),I=1,ARGDIMS(1))

      WRITE(CKEY,101)(IKEY(I),I=1,ARGDIMS(1))
101   FORMAT(2 A04)
      CALL QLXUDF(SCRAP,CKEY)
      RETURN
      END
!
      FUNCTION QLXVAL(KLE,ERR)  ! used by QLXASG and QLXCALL
      implicit none
      INTEGER QLXVAL
!
      CHARACTER *(*) KLE
      LOGICAL ERR
      INTEGER IND,VAL,DUM
!
      CALL QLXIND(IND,ERR)  ! get optional index [...]
!
      VAL = 0
      IF((.NOT. ERR))THEN
         CALL QLXADI(KLE,IND,VAL,DUM,ERR)  ! value of KEY or KEY[...]
      ENDIF 
      QLXVAL=VAL
!
      RETURN
      END
#if defined(WITH_EXPRESSIONS)
!**S/P QLXXPR TRAITER UNE EXPRESSION ARITHMETIQUE OU LOGIQUE
      SUBROUTINE QLXXPR(ERR)  ! process expression, store value into JVAL  THIS CODE IS BROKEN ON 64 BIT MACHINES
      use readlx_internals
      implicit none
      LOGICAL, intent(OUT) :: ERR
!       COMMON/QLXTOK1/LEN,TYPE,ZVAL,INEXPR
!       LOGICAL INEXPR
!       INTEGER LEN,TYPE,JVAL
!       REAL ZVAL
!       EQUIVALENCE (ZVAL,JVAL)
! !
! 
!       COMMON/QLXTOK2/TOKEN
!       CHARACTER *80 TOKEN
!
      integer, PARAMETER :: MAXTKNS=65, MAXOPS=30
      INTEGER TOKTYPE(MAXTKNS), NTOKEN
      INTEGER, dimension(MAXTKNS) :: TOKENS
      INTEGER NOPER
      CHARACTER(len=4) :: PILEOP(MAXOPS)
      LOGICAL UNARY, FINI, FIRST
      INTEGER PLEV, QLXPRI, BLEV, ITYP, LIMITES
      integer(kind=8) :: LOCVAR, LOCCNT
      EXTERNAL QLXPRI
!
  call ABORT      ! THIS ROUTINE IS BROKEN
      INEXPR = .TRUE.
      NTOKEN = 0
      PLEV = 0
      BLEV = 0
      UNARY = .TRUE.
      ERR = .FALSE.
      FINI = .FALSE.
      FIRST = .TRUE.
      NOPER = 1
      PILEOP(1) ='$'
23000 IF(( .NOT.FINI .AND. NTOKEN.LT.MAXTKNS .AND. NOPER.LT.MAXOPS .AND. .NOT.ERR))THEN
         IF((.NOT.FIRST))THEN
            CALL QLXTOK
         ENDIF 
         FIRST = .FALSE.
         IF((TYPE.EQ.0))THEN
            NTOKEN = NTOKEN + 1
            CALL QLXFND(TOKEN(1:8),LOCVAR,LOCCNT,LIMITES,ITYP)
            IF((ITYP.NE.0 .AND. ITYP.NE.1))THEN
               ERR=.TRUE.
            ENDIF 
            TOKENS(NTOKEN) = LOCVAR
            TOKTYPE(NTOKEN) = LIMITES + 1
            IF((.NOT. UNARY))THEN
               ERR=.TRUE.
            ENDIF 
            UNARY = .FALSE.
         ELSE 
            IF((TYPE.EQ.1 .OR. TYPE.EQ.2))THEN
               NTOKEN = NTOKEN + 1
               TOKENS(NTOKEN) = JVAL
               TOKTYPE(NTOKEN) = 0
               IF((.NOT. UNARY))THEN
                  ERR=.TRUE.
               ENDIF 
               UNARY = .FALSE.
            ELSE 
               IF((QLXPRI(TOKEN(1:4)).GT.0))THEN
                  IF((TOKEN(1:2).EQ.'( '))THEN
                     PLEV = PLEV + 1
                  ELSE 
                     IF((TOKEN(1:2).EQ.') '))THEN
                        PLEV = PLEV - 1
                     ELSE 
                        IF((TOKEN(1:2).EQ.'[ '))THEN
                           BLEV = BLEV + 1
                        ELSE 
                           IF((TOKEN(1:2).EQ.'] '))THEN
                              BLEV = BLEV - 1
                           ENDIF 
                        ENDIF 
                     ENDIF 
                  ENDIF 
                  IF((PLEV.LT.0 .OR. BLEV.LT.0))THEN
                     FINI = .TRUE.
                     CALL QLXBAK(TOKEN(1:1))
                     GOTO 23001
                  ENDIF 
                  IF((UNARY))THEN
                     IF((TOKEN(1:2).EQ.'+ '))THEN
                        TOKEN(1:2) = 'U+'
                     ELSE 
                        IF((TOKEN(1:2).EQ.'- '))THEN
                           TOKEN(1:2) = 'U-'
                        ELSE 
                           IF((TOKEN(1:2).NE.'( ' .AND. TOKEN(1:2).NE.'[ '))THEN
                              ERR=.TRUE.
                           ENDIF 
                        ENDIF 
                     ENDIF 
                  ENDIF 
                  UNARY = TOKEN(1:1).NE.')' .AND. TOKEN(1:1).NE.']'
                  CALL QLXRPN(TOKEN,TOKENS,MAXTKNS,NTOKEN,TOKTYPE,PILEOP,MAXOPS,NOPER,ERR)
               ELSE 
                  IF((TOKEN(1:1).EQ.',' .OR. TOKEN(1:1).EQ.'$'   .OR. TOKEN(1:2).EQ.':='))THEN
                     CALL QLXRPN('$',TOKENS,MAXTKNS,NTOKEN,TOKTYPE,PILEOP,MAXOPS,NOPER,ERR)
                     FINI = .TRUE.
                     CALL QLXBAK(TOKEN(1:1))
                  ELSE 
                     WRITE(6,'(A8,A)')TOKEN(1:8),' IS INVALID'
                     ERR = .TRUE.
                  ENDIF 
               ENDIF 
            ENDIF 
         ENDIF 
         GOTO 23000
      ENDIF 
23001 CONTINUE 
      IF( (PLEV.GT.0 .OR. .NOT.FINI .OR. BLEV.GT.0   .OR. NTOKEN.NE.1 ))THEN
         ERR = .TRUE.
      ENDIF 
      INEXPR = .FALSE.
      IF((.NOT.ERR))THEN
         TOKEN   = ' '
         JVAL   = TOKENS(1)
         IF((TOKTYPE(1).GT.0))THEN
            TYPE = 8                            ! flag result as an expression
         ELSE 
            IF((ABS(JVAL).LE.2147483647))THEN
               TYPE =1   ! integer
            ELSE 
               TYPE =2   ! float
            ENDIF 
         ENDIF 
      ENDIF 
      IF((ERR))THEN
         CALL QLXERR(81005,'QLXEXPR')
      ENDIF 
      RETURN
      END
#endif
      SUBROUTINE READLX(UNIT,KEND,KERR)
      use readlx_internals
      implicit none
!
!**S/R READLX - INTERPRETE DE DIRECTIVES
!
!AUTEUR   - M. VALIN
!
!LANGAGE  - RATFOR
!
!APPEL    - CALL READLX(UNIT,KEND,KERR)
!
!MODULES  - QLXFND,QLXTOK,QLXASG,QLXCALL,QLXERR,QLXFLSH
!
!ARGUMENTS
!         - UNIT - UNITE D'ENTREE
!         - KEND - 0 = TOUT EST NORMAL
!                  ?
!
!         - KERR -
!

!       COMMON/QLXTOK1/LEN,TYPE,ZVAL,INEXPR
!       LOGICAL INEXPR
!       INTEGER LEN,TYPE,JVAL
!       REAL ZVAL
!       EQUIVALENCE (ZVAL,JVAL)
!
!       COMMON/QLXTOK2/TOKEN
!       CHARACTER *80 TOKEN
!
!       COMMON /QLXBUFF/ NC,LAST,INPFILE,EOFL,NERR,SKIPFLG
!       COMMON /QLXBUFF/ CURREC,READREC,TMPFILE
!       INTEGER NC,LAST,INPFILE,NERR,SKIPFLG,CURREC,READREC,TMPFILE
!       LOGICAL EOFL
!       COMMON /QLXBUF2/ INLINE
!       CHARACTER *101 INLINE
!       CHARACTER * 20 LINEFMT
!       INTEGER KARMOT
!       COMMON /QLXFMT/ LINEFMT
!       COMMON /QLXFMT2/ KARMOT
!*

      EXTERNAL QLXNVAR,QLXPRNT,QLXUNDF,fnom
      INTEGER UNIT,KEND,fnom
      Integer(kind=8) :: LOCCNT,LOCVAR
      Integer IICNT
      INTEGER LIMITS,ITYP
      LOGICAL FIN,ERR
      integer, PARAMETER :: MAXSTRU=20
      INTEGER NXTELSE(0:2), NEXTIF(0:2), STYPE(MAXSTRU), SKIPF(MAXSTRU)
      INTEGER READBSE(MAXSTRU)
      INTEGER NSTRUC,ier,IDUM,KERRMAX,KERR
      character(len=128) :: nomscra
!

      DATA NXTELSE / 1, 0, 2/
      DATA NEXTIF  / 0, 2, 2/
!

!       DATA KARMOT /04/
!

      WRITE(LINEFMT,'(A,I2,A)') '(25 A',KARMOT,')'  ! format to print an input line, fix it if increasing length of line buffer
      KERRMAX = 999999
      KERR = 0    ! TEMPORAIRE
      IF((KERR.LT.0 ))THEN
         KERRMAX = MIN(ABS(KERR),KERRMAX)
      ENDIF 
      NC=1
      LAST=0
      INPFILE=UNIT
      EOFL=.FALSE.
      NERR=0
      FIN=.FALSE.
      INEXPR=.FALSE.
      STYPE(1) = 0
      SKIPF(1) = 0
      NSTRUC = 1
      CURREC=0
      READREC=0
      READBSE(1)=0
      nomscra='XXXXQLX'
      tmpfile = 0
      write(6,*)"================ NEW READLX ==================="
      ier = fnom(tmpfile,nomscra,'D77+SCRATCH+FMT',20)  ! open tempfile for 80 character records (fix this if increasing length of line buffer)
      CALL QLXINX(QLXPRNT,'PRINT',IDUM,0202,2)
      CALL QLXINX(QLXNVAR,'DEFINE',IDUM,0202,2)
      CALL QLXINX(QLXUNDF,'UNDEF',IDUM,0101,2)
23002 IF((.NOT.FIN .AND. NERR.LT.KERRMAX .AND. NSTRUC.LT.MAXSTRU))THEN
         SKIPFLG = SKIPF(NSTRUC)
         ERR=.FALSE.
         CALL QLXTOK
         IF((TYPE.EQ.0))THEN
            CALL QLXFND(TOKEN,LOCVAR,LOCCNT,LIMITS,ITYP)
            IF((ITYP.EQ.1 .AND. SKIPF(NSTRUC).EQ.0))THEN
               call get_content_of_location(LOCCNT,1,IICNT)
               CALL QLXASG(LOCVAR,IICNT,LIMITS,ERR)
               call set_content_of_location(LOCCNT,1,IICNT)
            ELSE 
               IF((ITYP.EQ.2 .AND. SKIPF(NSTRUC).EQ.0))THEN
                  CALL QLXCALL(LOCVAR,LOCCNT,LIMITS,ERR)
               ELSE 
                  IF((ITYP.EQ.3))THEN
                     NSTRUC = NSTRUC + 1
                     STYPE(NSTRUC) = ITYP
                     SKIPF(NSTRUC) = NEXTIF(SKIPF(NSTRUC-1))
                     IF((SKIPF(NSTRUC).EQ.0))THEN
                        CALL QLXTOK
                        IF((TOKEN(1:1).NE.'$'))THEN
#if defined(WITH_EXPRESSIONS)
                           CALL QLXXPR(ERR)
#else
                           ERR = .true.
#endif
                           IF((ERR))THEN
                              GOTO 23003
                           ENDIF 
                           IF((TYPE.EQ.8))THEN
                              call get_content_of_location(JVAL,1,JVAL)
                           ENDIF 
                           IF((IAND(JVAL,ishft(-1,32-(16))).EQ.0))THEN
                              SKIPF(NSTRUC) = 1
                           ENDIF 
                        ELSE 
                           CALL QLXBAK('$')
                        ENDIF 
                     ENDIF 
                     CALL QLXFLSH('$')
                  ELSE 
                     IF((ITYP.EQ.4))THEN
                        IF((STYPE(NSTRUC).NE.3))THEN
                           GOTO 23003
                        ENDIF 
                        STYPE(NSTRUC) = ITYP
                        SKIPF(NSTRUC) = NXTELSE(SKIPF(NSTRUC))
                        CALL QLXFLSH('$')
                     ELSE 
                        IF((ITYP.EQ.5))THEN
                           IF((STYPE(NSTRUC).NE.3 .AND. STYPE(NSTRUC).NE.4))THEN
                              GOTO 23003
                           ENDIF 
                           SKIPF(NSTRUC) = 0
                           NSTRUC = NSTRUC - 1
                           CALL QLXFLSH('$')
                        ELSE 
                           IF((ITYP.EQ.6))THEN
                              NSTRUC = NSTRUC + 1
                              STYPE(NSTRUC) = ITYP
                              SKIPF(NSTRUC) = NEXTIF(SKIPF(NSTRUC-1))
                              IF( (READREC.NE. 0))THEN
                                 READBSE(NSTRUC) = READREC -1
                              ELSE 
                                 READBSE(NSTRUC) = CURREC
                              ENDIF 
                              IF((SKIPF(NSTRUC).EQ.0))THEN
                                 CALL QLXTOK
                                 IF((TOKEN(1:1).NE.'$'))THEN
#if defined(WITH_EXPRESSIONS)
                                    CALL QLXXPR(ERR)
#else
                                    ERR = .true.
#endif
                                    IF((ERR))THEN
                                       GOTO 23003
                                    ENDIF 
                                    IF((TYPE.EQ.8))THEN
                                      call get_content_of_location(JVAL,1,JVAL)
                                    ENDIF 
                                    IF((IAND(JVAL,ishft(-1,32-(16))).EQ.0))THEN
                                       SKIPF(NSTRUC) = 1
                                    ENDIF 
                                 ELSE 
                                    CALL QLXBAK('$')
                                 ENDIF 
                              ENDIF 
                              CALL QLXFLSH('$')
                           ELSE 
                              IF((ITYP.EQ.7))THEN
                                 IF((STYPE(NSTRUC).NE.6))THEN
                                    GOTO 23003
                                 ENDIF 
                                 IF( (SKIPF(NSTRUC) .EQ. 0))THEN
                                    READREC = READBSE(NSTRUC)
                                 ENDIF 
                                 SKIPF(NSTRUC) = 0
                                 NSTRUC = NSTRUC - 1
                                 CALL QLXFLSH('$')
                              ELSE 
                                 IF((ITYP.GE.10 .AND. ITYP.LE.13 .AND. SKIPF(NSTRUC).EQ.0))THEN
                                    KERR=NERR
                                    KEND=ITYP-10
                                    FIN=.TRUE.
                                 ELSE 
                                    IF((SKIPF(NSTRUC).NE.0))THEN
                                       CALL QLXFLSH('$')
                                    ELSE 
                                       CALL QLXERR(21015,'READLX')
                                       ERR=.TRUE.
                                    ENDIF 
                                 ENDIF 
                              ENDIF 
                           ENDIF 
                        ENDIF 
                     ENDIF 
                  ENDIF 
               ENDIF 
            ENDIF 
         ELSE 
            CALL QLXERR(21016,'READLX')
            ERR=.TRUE.
         ENDIF 
         IF((ERR.AND.(TOKEN(1:1).NE.'$'.OR. TYPE.NE.4)))THEN
            CALL QLXFLSH('$')
         ENDIF 
         GOTO 23002
      ENDIF 
23003 CONTINUE 
      IF((NSTRUC.GT.1))THEN
         WRITE(6,*)' ERREUR DANS LA STRUCTURE DES BLOCS IF THEN ELSE'
         KERR = NERR + 1
         KEND = -1

!
      ENDIF 
      CLOSE(TMPFILE,STATUS='DELETE')
      RETURN
      END
