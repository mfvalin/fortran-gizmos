#if defined(SELF_TEST)
       program tata
      CHARACTER*8 LISTE(3),DEF1(3),DEF2(3)
      DATA LISTE /'L',   'IMFICH1','IMFICH2'/
      DATA DEF1  /'OUTPUT','TAPE1',  'TAPE2'  /
      DATA DEF2  /'OUTPUT','TAPE1',  'TAPE2'  /
*

      IPOS = -1
      CALL CCARD(LISTE,DEF1,DEF2,3,IPOS)
      do i = 1, 3
      print *,LISTE(i),DEF1(i),DEF2(i)
      enddo

       stop
       end
#endif
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
#define NKLEMAX 1024
        module ccard_private_sa
            USE ISO_C_BINDING
            PROCEDURE(), pointer, save :: callback => NULL()
        end module ccard_private_sa

        SUBROUTINE CCARD_callback_sa(p)
            USE ccard_private_sa
        external p

        call C_F_PROCPOINTER(C_FUNLOC(p), callback)
            return
        end

! RECUPERATION DES PARAMETRES D'APPEL A UN PROGRAMME

        SUBROUTINE CCARD(INCLE,DEF,VAL,N,II)
        use ccard_private_sa
        INTEGER N,II
        CHARACTER * (*) INCLE(N), DEF(N), VAL(N)

!ADAPTATION: JAMES CAVEEN

!OBJET(CCARD)   -   RECUPERATION DES PARAMETRES D'APPEL A UN PROGRAMME
!                   SI LE NOM D'UNE CLEF APPARAIT SEUL LORS DE LA SEQUENCE
!                   D'APPEL, ON LUI ATTRIBUE LA VALEUR CONTENUE DANS DEF
!                   SI LE NOM APPARAIT, SUIVI D'UNE OU DES VALEURS, ON LUI
!                   ATTRIBUE CES VALEURS.  SINON, LA CLEF PREND POUR VALEUR
!                   LE CONTENU INITIAL DE VAL.

!                   ON PEUT DONNER UNE VALEUR A UNE CLEF DES FACONS
!                   SUIVANTES:

!                   -CLEF VALEUR
!                   -CLEF VAL1:VAL2:VAL3:...
!                   -CLEF 2:3:-4
!                   -CLEF =-2:3:4
!                   -CLEF =-22
!                   -CLE=-22:33:...

!                   LORS DE LA SEQUENCE D'APPEL, TOUS LES PARAMETRES
!                   PRECEDANT LA PREMIERE CLEF RENCONTREE ET
!                   SUIVANT LES SIGNES -- SONT TRAITES EN MODE POSITIONEL.

!                   EXEMPLES D'UTILISATION:

!                   PROGNOM -CLEF1 -CLEF2 VAL2 -- POS1 POS2
!                   PROGNOM POS1 POS2 -CLEF1 -- POS3 POS4 ...
!                   PROGNOM -CLEF1 =-12:33 -CLEF2 12 =-34

!                   LORSQUE LE PREMIER PARAMETRE PASSE = -H/-h, CCARD IMPRIME
!                   SUR L'UNITE 6 LA SEQUENCE D'APPEL AU PROGRAMME.

!                   LA SEQUENCE D'APPEL ET LES VALEURS COURANTES DES CLEFS
!                   SERONT IMPRIMEES S'IL Y A ERREUR LORS DE L'APPEL.


!ARGUMENTS:
!       INCLE     ENTREE     -  NOM DES DIFFERENTES CLEFS A INITIALISER
!       DEF          "       -  DEUXIEME VALEUR DE DEFAUT
!       VAL       SORTIE     -  VALEUR FINALE ATTRIBUEE AUX CLEFS
!       N         ENTREE     -  NOMBRE DE CLEFS A INITIALISER
!       II        SORTIE     -  NOMBRE DE PARAMETRES POSITIONNELS
!                               (I.E. ASSOCIES AUX CLEFS "-")
!                            -  SI II > 0, ON RETOURNE LE NOMBRE DE
!                               PARAMETRES POSITIONELS.
!                            -  SI II = -111 ou +111, ON ARRETE DES LA PREMIERE
!                               ERREUR
!                            -  SI II <=0 & <> -111, ON NE RETOURNE PAS
!                               LE NOMBRE D'ARGUMENTS POSITIONELS ET ON
!                               CONTINUE MALGRE LES ERREURS

        INTEGER qqqobm_sa, LONGUEUR_SA, qqqoenv_sa
        EXTERNAL qqqobm_sa, qqqsap_sa, LOW2UP_SA, qqqtrns_sa, LONGUEUR_SA
        EXTERNAL qqqoenv_sa, qqexit_sa
        INTEGER I, POS, J, POSC, INDEX, CLLNG, POSMOIN, POSMOINC
! TYPE DE CLE -1=MINUS, 1=MAJUS, 0=PAREIL
        INTEGER CLTYPE(NKLEMAX)
        CHARACTER * 50 CLEUP, CLE(NKLEMAX), cletemp
        CHARACTER * 8192 ARGUP, ARG, CCARD_ARGS, ARGtemp
        CHARACTER * 60 Keyname
        LOGICAL PASFINI, PLANTE
        INTEGER STATUS, INTERNE
        Integer L_argenv

!       SI II = -111, LE PROGRAMME ARRETE DES LA PREMIERE
!       ERREUR RENCONTREE SINON, ON CONTINUE

        plante = abs(ii) .eq. 111
        call getenv('CCARD_OPT',ccard_args)
        L_argenv = LONGUEUR_SA(ccard_args)
        IF (L_argenv .gt. 0) THEN
          IF (ccard_args(1:L_argenv) .eq. 'ABORT') THEN
            plante = .true.
          ENDIF
        ENDIF
        call getenv('CCARD_ARGS',ccard_args)
        L_argenv = LONGUEUR_SA(ccard_args)


!       INITIALISER LE VECTEUR DE TYPE DE CLEFS

        DO 20 I = 1, N
            cletemp = incle(i)
            IF (TRIM(incle(i)) .ne. TRIM(cletemp)) THEN
              write(*,777) 'CCARD erreur: nom de la cle #',i,
     $                     ' > limite de 50 caracteres'
 777          format(a,i4,a)
              if (plante) call qqexit_sa(21)
            ENDIF
            CALL LOW2UP_SA(INCLE(I),CLEUP)
            DO 10 J =50,1,-1
                IF(CLEUP(J:J) .NE. ' ') THEN
                    INDEX = J
                    GOTO 11
                ENDIF
 10         CONTINUE
 11         CONTINUE

            IF(CLEUP(INDEX:INDEX) .EQ. '.') THEN
                CLTYPE(I) = 0
                CLEUP(INDEX:INDEX) = ' '
            ELSE IF (CLEUP(INDEX:INDEX) .EQ. '_') THEN
                CLTYPE(I) = -1
                CLEUP(INDEX:INDEX) = ' '
            ELSE IF(CLEUP(INDEX:INDEX) .EQ. ':') THEN
                CLTYPE(I) = 2
                CLEUP(INDEX:INDEX) = ' '
            ELSE
                CLTYPE(I) = 1
            ENDIF
            CLE(I) = CLEUP
 20     CONTINUE

!       TROUVER LA POSITION DE LA PREMIERE CLEF - DANS LA LISTE CLE

        POSMOIN = 0
        POSMOINC = 0
        DO 30 J=1,N
            IF(CLE(J) .EQ. '-') THEN
                  POSMOINC = J
                  POSMOIN  = J
                  GOTO 80
            ENDIF
 30     CONTINUE

 80     CONTINUE

        POS  = 0
        POSC = 0
        INTERNE   = 0
        PASFINI = .TRUE.
 100    CONTINUE
        IF (PASFINI) THEN
          if (L_argenv .gt. 0) then
            status = qqqoenv_sa(arg,ccard_args,L_argenv)
          else
            STATUS = qqqobm_sa(ARG,cltype(pos))
          endif
          IF(STATUS .EQ. 1) THEN
            CALL LOW2UP_SA(ARG,ARGUP)
            DO 110 J = 1,N
              CLEUP = CLE(J)
              IF (trim(ARGUP) .EQ. trim(CLEUP)) THEN
                POSC = J
                POS = J
                CALL qqqtrns_sa(VAL(POS), DEF(POS),CLTYPE(POS))
!              print *,'Debug+ ccard cleup=',cleup,' val(pos)=',val(pos)
                write(keyname,109) '%%'//trim(cle(posc)),posc-pos,'%%'
 109            format(a,i4.4,a)
!               call c_set_appl_var(Keyname,val(pos))
                GOTO 201
              ENDIF
 110        CONTINUE
            CLLNG = LONGUEUR_SA(ARGUP)
            PRINT *,' *** ERREUR: cle ',ARGUP(1:CLLNG),' non reconnue'
            CALL qqqsap_sa(CLE,DEF,VAL,N)
            IF(PLANTE) THEN
               CALL qqexit_sa(22)
            ENDIF
 201        CONTINUE
          ELSE IF(STATUS .EQ. 2) THEN
            IF (POSC .NE. 0 .AND. CLE(POS) .EQ. CLE(POSC)) THEN
              CALL qqqtrns_sa(ARGtemp,ARG,CLTYPE(POSC))
              VAL(POSC) = ARGtemp
!              print *,'Debug+ ccard cle(posc)=',cle(posc),' ARGtemp=',trim(ARGtemp)
!              print *,'Debug+ ccard val(posc)=',val(posc)
              write(keyname,109) '%%'//trim(cle(posc)),posc-pos,'%%'
!             call c_set_appl_var(Keyname,ARGtemp)
              POSC = POSC + 1
            ELSE
              PRINT *,' *** ERREUR: debordement de liste'
              CALL qqqsap_sa(CLE,DEF,VAL,N)
              IF(PLANTE) THEN
                   CALL qqexit_sa(23)
              ENDIF
            ENDIF
          ELSE IF(STATUS .EQ. 3) THEN
            IF (POSMOINC .NE. 0 .AND. CLE(POSMOIN) .EQ. CLE(POSMOINC))
     %        THEN
              CALL qqqtrns_sa(ARGtemp,ARG,CLTYPE(POSMOINC))
              VAL(POSMOINC) = ARGtemp
              write(keyname,109)
     $             '%%'//trim(cle(posmoinc)),posmoinc-pos,'%%'
!             call c_set_appl_var(Keyname,ARGtemp)
            ELSE
              PRINT *,' *** ERREUR: debordement de liste'
              PRINT *,'         ou  mode positionnel non permis'
              CALL qqqsap_sa(CLE,DEF,VAL,N)
              IF(PLANTE) THEN
                   CALL qqexit_sa(24)
              ENDIF
            ENDIF
            POSMOINC = POSMOINC + 1
            INTERNE = INTERNE + 1
          ELSE IF(STATUS .EQ. 5) THEN
              CALL qqqsap_sa(CLE,DEF,VAL,N)
              if(associated(callback)) then
                print *,''
                call callback
              endif
              CALL qqexit_sa(0)
          ELSE
             PASFINI = .FALSE.

          ENDIF
          GOTO 100
        ENDIF

!       RETOURNER LE NOMBRE D'ARGUMENTS ASSOCIES A LA CLEF - SI DEMANDE
        IF(II .GT. 0) THEN
           II = INTERNE
        ENDIF
        RETURN
        END

!**S/P  qqqobm_sa - OBTENIR UN NOM DE CLEF OU UNE VALEUR D'UN ARGUMENT

        FUNCTION qqqobm_sa(ARG,cltype)

        INTEGER qqqobm_sa
        CHARACTER * 8192 ARG
        integer cltype

!AUTEUR          J.CAVEEN, JANVIER 1991

!OBJET(qqqobm_sa)
!       FONCTION QUI PERMET D'ALLER CHERCHER LES ARGUMENTS D'UNE SEQUENCE
!       D'APPEL A UN PROGRAMME ET D'EN EXTRAIRE LES NOMS DE CLEFS ET LES
!       VALEURS A DONNER A CES CLEFS.
!       LA FONCTION qqqobm_sa RETOURNE:
!            - UNE VALEUR DE 1 SI ARG CONTIENT UN NOM DE CLEF
!            - UNE VALEUR DE 2 SI ARG CONTIENT UNE VALEUR A DONNER A UNE CLEF
!            - UNE VALEUR DE 3 SI ARG CONTIENT UN ARGUMENT POSITIONEL
!            - UNE VALEUR DE 5 SI ON DEMANDE LA SEQUENCE D'APPEL
!            - UNE VALEUR DE 0 LORSQUE TOUT EST FINI

!ARGUMENT:
!         ARG    SORTIE     NOM DE CLEF OU VALEUR RETOURNE

!*
        INTEGER LONGUEUR_SA
        EXTERNAL LONGUEUR_SA

        LOGICAL PASDCLE, PUDCLE, PREMIER
        INTEGER ARGNUM, IINDEX, INDFIN, NARG, I, INDEB, J
        CHARACTER *8192 ARGUP
        character *1 delim

        SAVE ARGUP
        SAVE PASDCLE, PUDCLE, PREMIER, ARGNUM, NARG, IINDEX, INDFIN
        DATA PASDCLE, PUDCLE, PREMIER /.TRUE., .FALSE., .TRUE./
        DATA ARGNUM, NARG, IINDEX, INDFIN /0, 0, 0, 0/


        if (cltype .eq. 2) then
          delim = '='
        else
          delim = ':'
        endif

 100    qqqobm_sa = 0
        DO 120 I = 1, 8192
          ARG(I:I) = ' '
 120    CONTINUE

        IF(PREMIER) THEN
            NARG = IARGC()
            PREMIER = .FALSE.
        ENDIF

        IF(IINDEX .GE. INDFIN) THEN
!            #  ALLER CHERCHER UN ARGUMENT

             ARGNUM = ARGNUM + 1
             IF (ARGNUM .GT. NARG)   RETURN

             DO 150 I = 1,8192
                 ARGUP(I:I) = ' '
 150         CONTINUE

             CALL GETARG(ARGNUM,ARGUP)
             IF(((NARG .EQ. 1) .AND. ((INDEX(ARGUP,'-H ') .NE. 0) .OR.
     %          (INDEX(ARGUP,'-h ') .NE. 0)
     %          .OR. (INDEX(ARGUP,'-HELP') .NE. 0)
     %          .OR. (INDEX(ARGUP,'-help') .NE. 0) ))) THEN
                 qqqobm_sa = 5
                 RETURN
             ENDIF

             IINDEX = 1

!            TROUVER LA FIN DE ARGUP

             INDFIN = LONGUEUR_SA(ARGUP)
         ENDIF


!         EXTRAIRE UN NOM DE CLEF OU UNE VALEUR


        IF((IINDEX .EQ. 1) .AND.  (ARGUP(1:1) .EQ. '-') .AND.
     %     (.NOT. PUDCLE)) THEN

!         NOM DE CLEF

            PASDCLE = .FALSE.
            qqqobm_sa = 1
            IINDEX = 2
            IF(ARGUP(2:2) .EQ. '-') THEN
                PUDCLE = .TRUE.
                GO TO 100
!               CHERCHER PROCHAIN ARG POSITIONEL
            ENDIF
        ELSE
! VALEUR A DONNER
            IF(PUDCLE .OR. PASDCLE) THEN
! ARGUMENT POSITIONEL
                 qqqobm_sa = 3
                 ARG = ARGUP
                 IINDEX = INDFIN
                 RETURN
            ELSE
                 qqqobm_sa = 2
            ENDIF
            IF((ARGUP(IINDEX:IINDEX) .EQ. delim) .OR.
     %         (ARGUP(IINDEX:IINDEX) .EQ. '=')) then
                       IINDEX = IINDEX + 1
            endif
        ENDIF

        INDEB = IINDEX
        J = 1
        DO 200 I= INDEB ,INDFIN
            IF((ARGUP(I:I) .EQ. '=') .OR. (ARGUP(I:I) .EQ. delim)) THEN
                  GOTO 500
            ELSE
                 ARG(J:J) = ARGUP(I:I)
                 IINDEX = IINDEX +1
                 J = J + 1
            ENDIF
 200    CONTINUE

 500    CONTINUE

        RETURN
        END

!**S/P  qqqoenv_sa - OBTENIR UN NOM DE CLEF OU UNE VALEUR D'UN ARGUMENT
!                 A PARTIR D'UNE VARIABLE D'ENVIRONNEMENT CONTENANT LA
!                 SEQUENCE D'APPEL COMPLETE

        INTEGER FUNCTION qqqoenv_sa(ARGP,CCARD_ARGS,L)
        IMPLICIT NONE
        CHARACTER * 8192 ARGP, CCARD_ARGS
        INTEGER L

!AUTEUR          M. Lepine,  Fevrier 2003

!OBJET(qqqoenv_sa)
!       FONCTION QUI PERMET D'ALLER CHERCHER LES ARGUMENTS D'UNE SEQUENCE
!       D'APPEL A UN PROGRAMME ET D'EN EXTRAIRE LES NOMS DE CLEFS ET LES
!       VALEURS A DONNER A CES CLEFS.
!       LA FONCTION qqqoenv_sa RETOURNE:
!            - UNE VALEUR DE 1 SI ARG CONTIENT UN NOM DE CLEF
!            - UNE VALEUR DE 2 SI ARG CONTIENT UNE VALEUR A DONNER A UNE CLEF
!            - UNE VALEUR DE 3 SI ARG CONTIENT UN ARGUMENT POSITIONEL
!            - UNE VALEUR DE 5 SI ON DEMANDE LA SEQUENCE D'APPEL
!            - UNE VALEUR DE 0 LORSQUE TOUT EST FINI

!ARGUMENT:
!         ARGP    SORTIE     NOM DE CLEF OU VALEUR RETOURNE

!*
        Integer pos, i, ic, indfin
        Logical debut, pudcle
        character c, quotechar
        character * 8192 arg
        integer  LONGUEUR_SA
        external LONGUEUR_SA
        SAVE        pos,debut
        DATA        debut, pudcle /.true., .false./

        if (debut) then
          pos = 1
          debut = .false.
        endif

 100    continue
        i = 1
        arg = ' '
        argp = ' '

        if (pos > L) then
           qqqoenv_sa = 0        ! termine, fin de la sequence d'appel
           return
        endif

        c = ccard_args(pos:pos)
        getarg: DO                            ! obtenir la prochaine cle, valeur
          if ((pos > L) .or. (c .eq. ' ') .or. (c .eq. ':')) EXIT getarg
          if ((c .ne. '''') .and. (c .ne. '"')) then
            arg(i:i) = c
            pos = pos +1
            i = i + 1
            if (pos <= L) c = ccard_args(pos:pos)
          else
            quotechar = c
            pos = pos + 1
            c = ccard_args(pos:pos)
            quote: DO
                       if (c .eq. quotechar) EXIT quote
                  if (pos > L) then
                  print *,'CCARD, qqqoenv_sa error: unmatched quote'
                  EXIT quote
                endif
                arg(i:i) = c
                 i = i + 1
                pos = pos +1
                if (pos <= L) c = ccard_args(pos:pos)
            END DO quote
            pos = pos +1
            if (pos <= L) c = ccard_args(pos:pos)
          endif
        END DO getarg

        indfin = LONGUEUR_SA(arg)

        if ((arg(1:1) .eq. '-') .and. (.not. pudcle)) then
           qqqoenv_sa = 1           ! nom de cle
           ic = 2                ! position de copie apres '-'
           if (arg(2:2) .eq. '-') then     ! -- passage en mode positionel
              pudcle = .true.
              goto 100                     ! prochain argument positionel
           endif
        else
           if (pudcle) then                ! argument positionel
              qqqoenv_sa = 3
              ic = 1
           else
              qqqoenv_sa = 2                  ! valeur
              ic = 1
           endif
           if (arg(1:1) .eq. '=') ic = 2   ! passer le caractere d'escape
        endif
        argp = arg(ic:indfin)
        if ((qqqoenv_sa == 1) .and. (indfin == 2) .and.
     % ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-H') .or.
     %  (arg(1:5) .eq. '-help') .or. (arg(1:5) .eq. '-HELP') )) then
          qqqoenv_sa = 5                      ! sequence d'appel demande
        endif
        pos = pos + 1          ! positionnement au debut du prochain argument

        return
        end

!**S/P - qqqtrns_sa - TRADUIRE ET TRANSFERER UNE VALEUR SELON LE TYPE

        SUBROUTINE qqqtrns_sa(SORTI,ENTRE,TYPE)

        CHARACTER*(*) SORTI,ENTRE
        INTEGER TYPE

!AUTEUR:   JAMES CAVEEN,  JUILLET 1991

!OBJET(qqqtrns_sa) - TRADUIRE UNE VALEUR EN MAJUSCULES/MINUSCULE SELON
!                 LE TYPE DE CLE ET TRANSFERE LE RESULTAT DANS SORTI

!ARGUMENTS    SORTI  SORTIE -  NOM RESULTANT DE LA TRANSFORMATION
!             ENTRE  ENTREE - NOM A TRADUIRE
!             TYPE      "   - TYPE DE TRADUCTION A APPLIQUER

!*
        EXTERNAL LOW2UP_SA, UP2LOW_SA

        IF(TYPE .EQ. 1) THEN
           CALL LOW2UP_SA(ENTRE,SORTI)
        ELSE IF(TYPE .EQ. -1) THEN
           CALL UP2LOW_SA(ENTRE,SORTI)
        ELSE
           SORTI = ENTRE
        ENDIF
        RETURN
        END

!**S/P qqqsap_sa - IMPRIMER LA SEQUENCE D'APPEL AU PROGRAMME

        SUBROUTINE qqqsap_sa(CLE,DEF,VAL,N)

        INTEGER N
        CHARACTER* (*) CLE(N), DEF(N), VAL(N)


!AUTEUR:  JAMES CAVEEN, JUILLET 1991

!OBJET(qqqsap_sa) - SOUS-PROGRAMME SERVANT A IMPRIMER LA SEQUENCE D'APPEL
!                AU PROGRAMME PRESENTEMENT EN EXECUTION.
!                qqqsap_sa IMPRIME SUR L'UNITE 6:
!                   LE NOM DU PROGRAMME, LES DIFFERENTS NOM DE CLEFS
!                   ET LEUR PREMIERE ET DEUXIEME VALEUR DE DEFAUT.

!ARGUMENTS:
!        CLE      ENTREE  - TABLEAU CONTENANT LE NOM DES N CLEFS
!        DEF      ENTREE  - TABLEAU CONTENANT LA DEUXIEME VALEUR DE DEFAUT
!        VAL      ENTREE  - TABLEAU CONTENANT LA PREMIERE VALEUR DE DEFAUT
!                           OU LA VALEUR COURANTE DE LA CLEF
!        N        ENTREE  - NOMBRE DE CLEFS


        INTEGER LONGUEUR_SA
        EXTERNAL LONGUEUR_SA

        CHARACTER*8192 LENOM
        INTEGER I,NOMLNG,CLELNG,DEFLNG,VALLNG
        character*128 clepre
        integer repcle

!       ON OBTIENT LE NOM DU PROGRAMME APPELANT
        CALL GETARG(0,LENOM)

        WRITE(6,100)

        NOMLNG = LONGUEUR_SA(LENOM)
        WRITE(6,*)LENOM(1:NOMLNG)
        clepre = ' '
        repcle = 0

        DO 50 I =1,N
           if(trim(clepre) == trim(CLE(I))) then
             repcle = repcle + 1
           else
             if(repcle > 0) then
               print 300,'     [repeated',repcle,' time(s)]'
             endif
             repcle = 0
             clepre = trim(CLE(I))
             CLELNG = LONGUEUR_SA(CLE(I))
             DEFLNG = LONGUEUR_SA(DEF(I))
             VALLNG = LONGUEUR_SA(VAL(I))
             WRITE(6,200) CLE(I)(1:CLELNG),VAL(I)(1:VALLNG),
     %                  DEF(I)(1:DEFLNG)
           endif
 50     CONTINUE
        if(repcle > 0) then
          print 300,'     [repeated',repcle,' more time(s)]'
        endif

 100    FORMAT(' *** SEQUENCE D''APPEL ***'//)
 200    FORMAT('     -',A,' [',A,':',A,']')
 300    FORMAT(A,I4,A)
        RETURN
        END
      INTEGER FUNCTION LONGUEUR_SA(NOM)
      implicit none
      CHARACTER * (*) NOM
*
      INTEGER LNG,I
*
      LNG = LEN(NOM)
      DO 10 I = LNG,1,-1
         IF (NOM(I:I) .EQ. ' ') THEN
            LNG = LNG - 1
         ELSE
            GOTO 20
         ENDIF
 10   CONTINUE
 20   CONTINUE
      LONGUEUR_SA = LNG
      RETURN
      END

*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */
        SUBROUTINE UP2LOW_SA(S1,S2)
        CHARACTER * (*) S1,S2
        INTEGER I,L,L2
        L = MIN(LEN(S1),LEN(S2))
        L2 = LEN(S2)
        DO 10 I = 1,L
          IF ((ICHAR(S1(I:I)).GE.65) .AND. (ICHAR(S1(I:I)).LE.90)) THEN
            S2(I:I) = CHAR(ICHAR(S1(I:I)) + 32)
          ELSE
            S2(I:I) = S1(I:I)
          ENDIF
 10     CONTINUE
	DO 15 I = L+1,L2
	  S2(I:I) = ' '
 15     CONTINUE
        RETURN
*
        ENTRY LOW2UP_SA(S1,S2)
        L = MIN(LEN(S1),LEN(S2))
        L2 = LEN(S2)
        DO 20 I = 1,L
          IF ((ICHAR(S1(I:I)).GE.97) .AND. (ICHAR(S1(I:I)).LE.122)) THEN
            S2(I:I) = CHAR(ICHAR(S1(I:I)) - 32)
          ELSE
            S2(I:I) = S1(I:I)
          ENDIF
 20     CONTINUE
        DO 30 I = L+1,L2
          S2(I:I) = ' '
 30     CONTINUE
        RETURN
        END
       subroutine qqexit_sa(i)
       integer i
       call exit(i)
       return
       end
