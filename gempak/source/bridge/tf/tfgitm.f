	SUBROUTINE TF_GITM ( irtarr, jrtarr, itype, iveday, ibday, 
     +                       ieday, ivbhr, ntimes, ibtime, ietime,
     +                       tmidxc, tmidxr, indxb, indxe, good,  iret )
C************************************************************************
C* TF_GFTM								*
C*									*
C* This routine returns the indices associated with the forecast hours  *
C*									*
C* TF_GITM ( IRTARR, JRTARR, ITYPE, IVEDAY, IBDAY, IEDAY,               *
C*           IVBHR, NTIMES, IBTIME, IETIME, TMIDXC,                     *
C*           TMIDXR, INDXB, INDXE, GOOD, IRET )                         *
C*									*
C* Input parameters:							*
C*      IRTARR (5)      INTEGER         Integer report time array       *
C*      JRTARR (5)      INTEGER         Integer initial fcst time array *
C*      ITYPE           INTEGER         Type of next change indicator   *
C*	IVEDAY    	INTEGER		The ending valid day in bul repo*
C*	IBDAY     	INTEGER		The beginning day in new FM repo*
C*	IEDAY     	INTEGER		The ending day in new FM repo   *
C*	IVBHR     	INTEGER		The valid begining hours        *
C*      NTIMES          INTEGER         Number of forecast times        *
C*	IBTIME    	INTEGER		The begining hours              *
C*	IETIME    	INTEGER		The ending hours                *
C*      TMIDXR          LOGICAL         True if report time in a new FM *
C*      TMIDXC          LOGICAL         True if CFI is in a new 30 hrFM *
C*									*
C* Output parameters:							*
C*      INDXB           INTEGER         The beginning hour of indice    *
C*      INDXE           INTEGER         The endning hour of indice      *
C*      GOOD            LOGICAL         The data processing flag        *
C*	IRET      	INTEGER		=-5, not a good indice          *
C*									*
C**									*
C* Log:									*
C* L.LIN/NCEP		4/08	Created                                 *
C* S. Jacobs/NCEP	4/09	Added PROB30/40 processing		*
C************************************************************************
C*
        INTEGER         irtarr (*), jrtarr (*)
        INTEGER         ibday, ieday
        LOGICAL         tmidxr, tmidxc, good
C
C------------------------------------------------------------------------
C
        iret = 0
C
        IF ( .not. tmidxc ) THEN
C
C*         Data is not a new forecast change indicator
C
           IF ( ( itype .eq. 0 ) .and. ( .not. tmidxr ) ) THEN
C
C*             Data is a report time in old YYGGgg format 
C
               indxb = ibtime - ivbhr + 1
               indxe = ietime - ivbhr
               IF ( indxb .lt. 1 ) indxb = indxb + 24
               IF ( indxe .lt. 1 ) indxe = indxe + 24
            ELSE IF ( ( itype .eq. 0 ) .and. tmidxr ) THEN
C
C*             Data is a report time in new YYGG/yygg format 
C*             Get the ending indice
C
               CALL TF_CTIM ( irtarr, jrtarr, iveday, ietime,
     +                        ltimes, good )
C
               indxe = ltimes
               IF ( indxe .gt. 30 ) indxe = 30
               indxb = 1
            ELSE IF ( ( tmidxr ) .and. ( itype .ne. 0 ) ) THEN
C
C*             Report time is in new format, but FCI is in old
C
               indxb = ibtime - ivbhr + 1
               IF ( itype .eq. 1 ) THEN
                   indxe = ntimes
               ELSE IF ( itype .eq. 2 ) THEN
                  indxb = ibtime + 1
                  indxe = ntimes
               ELSE
                  indxe = ietime - ivbhr
               ENDIF
               IF ( indxb .lt. 1 ) indxb = indxb + 24
               IF ( indxe .lt. 1 ) indxe = indxe + 24
               IF ( indxe .lt. indxb ) THEN
                    indxe = indxe + 24
                    IF ( indxe .gt. ntimes ) indxe = ntimes
               END IF
            ELSE IF ( ( .not. tmidxr ) .and.
     +               ( itype .ne. 0 ) ) THEN
C
C*             Report time is in old format and also FCI
C
               indxb = ibtime - ivbhr + 1
               indxe = ietime - ivbhr
               IF ( indxb .lt. 1 ) indxb = indxb + 24
               IF ( indxe .lt. 1 ) indxe = indxe + 24
           ENDIF
        ELSE
           IF ( itype .eq. 1 ) THEN
C
C*             The FM is in new format "FMYYGGgg"
C*             Get the beginning indice
C
               CALL TF_CTIM ( irtarr, jrtarr, ibday, ibtime,
     +                        ltimes, good )
               indxb = ltimes + 1
               indxe = ntimes
            ELSE IF ( itype .eq. 2 ) THEN
C
C*             The BECMG is in new format "BECMG YYGG/yygg"
C*             Calculate the beginning indice
C
               CALL TF_CTIM ( irtarr, jrtarr, ibday, ibtime,
     +                        ltime1, good )
               CALL TF_CTIM ( irtarr, jrtarr, ieday, ietime,
     +                        ltime2, good )
               indxb = NINT ( FLOAT (ltime1 + ltime2) * .5 - .2) + 1
               IF ( ( indxb .lt. 0 ) .or. ( indxb .ge. ntimes ) )
     +             good = .false.
               indxe = ntimes
            ELSE IF ( ( itype .eq. 3 ) .or.
     +		      ( itype .eq. 4 ) .or.
     +		      ( itype .eq. 5 ) ) THEN
C
C*             The TEMPO or PROB are in new format
C*	       "TEMPO YYGG/yygg" or "PROBxx YYGG/yygg"
C*             Get the beginning indice
C
               CALL TF_CTIM ( irtarr, jrtarr, ibday, ibtime,
     +                        ltimes, good )
               IF ( ltimes .lt. 0 ) good = .false.
               indxb = ltimes + 1
C
C*             Get the ending indice
C
               CALL TF_CTIM ( irtarr, jrtarr, ieday, ietime,
     +                        ltimes, good )
               indxe = ltimes
               IF ( indxe .gt. ntimes ) THEN
                  good = .false.
                  indxe = ntimes
               END IF
           END IF
        END IF
C
        IF ( ( indxb .le. 0) .or. ( indxe .gt. ntimes ) ) THEN
             good = .false.
             iret = -1
        END IF
C
        RETURN
        END
