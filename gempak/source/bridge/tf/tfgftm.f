	SUBROUTINE TF_GFTM ( irtarr, jrtarr, lens, ivbday, iveday, 
     +                       ivbhr, ivehr, ntimes, ibtime, ietime,
     +                       tmidxr, iret )
C************************************************************************
C* TF_GFTM								*
C*									*
C* This routine returns a number for the forecast times.		*
C*									*
C* TF_GFTM ( IRTARR, JRTARR, LENS, IVBDAY, IVEDAY, IVBHR, IVEHR,        *
C*           NTIMES, IBTIME, IETIME, TMIDXR, IRET )                     *
C*									*
C* Input parameters:							*
C*      IRTARR (5)      INTEGER         Integer report time array       *
C*      JRTARR (5)      INTEGER         Integer initial fcst time array *
C*	LENS  		INTEGER		The length of the report time   *
C*	IVBDAY    	INTEGER		The beginning valid day         *
C*	IVEDAY    	INTEGER		The ending valid day            *
C*	IVBHR     	INTEGER		The valid beginning hours       *
C*	IVEHR     	INTEGER		The valid ending hours          *
C*									*
C* Output parameters:							*
C*      NTIMES          INTEGER         Number of forecast times        *
C*      IBTIME          INTEGER         The beginning forecast hour     *
C*      IETIME          INTEGER         The endning forecast hour       *
C*      TMIDXR          LOGICAL         True if report time in new form *
C*	IRET      	INTEGER		=-1, not a valid time           *
C*									*
C**									*
C* Log:									*
C* L.LIN/NCEP		4/08	Created                                 *
C* L.LIN/NCEP		11/08	return -1 if fcst time more than 30     *
C************************************************************************
C*
        INTEGER         irtarr (*), jrtarr (*)
        INTEGER         lirtar (5), ljrtar (5)
        INTEGER         ie1tar(5), ie2tar(5), ier1, ier2
        INTEGER         maxhr
        LOGICAL         tmidxr
C
        DATA            maxhr  / 30 /
C
C*      flag tmidxr will be set to true if report period in a new 
C*      format such as YYGG/YYgg
C
        iret = 0
C
C*      Get the time group for 30 hours away from the report time
C
        CALL TI_ADDM ( irtarr, 1800, ie1tar, ier1 )
        IF ( ier1 .ne. 0) THEN
           iret = -1
        ENDIF
C
C*      Get the time group for 24 hours away from the report time
C
        CALL TI_ADDM ( irtarr, 1440, ie2tar, ier2 )
        IF ( ier2 .ne. 0) THEN
           iret = -1
        ENDIF
C
C
C*      Get the initial forecast time array.
C
        CALL DC_ITIM ( irtarr, ivbday, ivbhr, 0, jrtarr, ier )
        CALL TI_MDIF ( irtarr, jrtarr, nmin, ier )
C
C*      Discard reports with initial forecast time more than 7 hours
C*      earlier or 23 hours later than report time.
C
        IF ( ier .eq. 0 ) THEN
            IF ( ( nmin .lt. -1380 ) .or. ( nmin .gt. 420 ) )  iret = -1
          ELSE
            iret = -1
        END IF
C
        IF ( iret .eq. -1 ) RETURN
C
C*      Get the forecast beginning and ending valid times.
C
        IF ( lens .eq. 6 ) THEN
C
C*      Valid time format is in YYGGgg with 6 charaters
C*      It will not past 24 hour
C
           IF ( ivehr .gt. 50 ) ivehr = ivehr - 50
           IF ( ivehr .eq. 24 ) ivehr = 0
           ibtime = ivbhr
           ietime = ivehr
           ntimes = ietime - ibtime
           IF ( ntimes .le. 0 )  ntimes = ntimes + 24
           IF ( ntimes .gt. 24 ) ntimes = 24
C
        ELSE IF ( lens .eq. 9 ) THEN
C
C*      Valid time format is in YYGG/YYgg with 9 charaters
C*      It will not past 30 hour
C
           IF ( ivehr .gt. 50 ) ivehr = ivehr - 50
           ibtime = ivbhr
           ietime = ivehr
C
C*         calculate the forecast times by two indivual times
C
           DO jj = 1,5
              lirtar(jj) = irtarr(jj)
              ljrtar(jj) = jrtarr(jj)
           END DO
C
           lirtar(3) = ivbday
           lirtar(4) = ibtime
           ljrtar(3) = iveday
           ljrtar(4) = ietime
C
C*         Decide the year and month from the valid day
C
           IF ( ivbday .eq.  ie2tar(3) ) THEN
              lirtar(1) = ie2tar(1)
              lirtar(2) = ie2tar(2)
           ELSE IF ( ivbday .eq.  ie1tar(3) ) THEN
              lirtar(1) = ie1tar(1)
              lirtar(2) = ie1tar(2)
           ENDIF
C
           IF ( iveday .eq.  ie2tar(3) ) THEN
              ljrtar(1) = ie2tar(1)
              ljrtar(2) = ie2tar(2)
           ELSE IF ( iveday .eq.  ie1tar(3) ) THEN
              ljrtar(1) = ie1tar(1)
              ljrtar(2) = ie1tar(2)
           END IF
C
           CALL TI_MDIF(ljrtar, lirtar, ldiff, lret)
           ltimes = ldiff/60
           ntimes = ltimes
           tmidxr = .true.
        END IF
C
        IF ( ntimes .gt. maxhr ) iret = -1
C 
        RETURN
        END
