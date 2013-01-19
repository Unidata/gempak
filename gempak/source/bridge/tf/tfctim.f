	SUBROUTINE TF_CTIM ( irtarr, jrtarr, iday, ihour, 
     +                       ltimes, good )
C************************************************************************
C* TF_GFTM								*
C*									*
C* This routine returns an indice time calculated by a given day        *
C*  time group from the initial forecast day time group.                *
C*									*
C* TF_CTIM ( IRTARR, JRTARR, IDAY, IHOUR, LTIMES, GOOD )                *
C*									*
C* Input parameters:							*
C*      IRTARR (5)      INTEGER         Integer report time array       *
C*      JRTARR (5)      INTEGER         Integer initial fcst time array *
C*	IDAY     	INTEGER		The given day in the time group *
C*	IHOUR    	INTEGER		The given hour in the time group*
C*									*
C* Output parameters:							*
C*      ltimes          INTEGER         The associated indice time      *
C*      good            LOGICAL         The data processing flag        *
C*									*
C**									*
C* Log:									*
C* L.LIN/NCEP		08/04	Created                                 *
C************************************************************************
C*
        INTEGER         irtarr (*), jrtarr(*)
        INTEGER         lirtar (5), ljrtar (5)
        INTEGER         ie1tar(5), ie2tar(5)
        INTEGER         iday
        LOGICAL         good
C
C*      Get the time group for 30 hours away from the report time
C
        CALL TI_ADDM ( irtarr, 1800, ie1tar, ier1 )
C
C*      Get the time group for 24 hours away from the report time
C
        CALL TI_ADDM ( irtarr, 1440, ie2tar, ier2 )
C
        DO jj = 1,5
           lirtar(jj) = jrtarr(jj)
           ljrtar(jj) = jrtarr(jj)
        END DO
C
        ljrtar(3)=iday
        ljrtar(4)=ihour
C
C*      Decide the year and month from the given day
C
        IF ( iday  .eq.  ie2tar(3) ) THEN
             ljrtar(1) = ie2tar(1)
             ljrtar(2) = ie2tar(2)
          ELSE IF ( iday  .eq.  ie1tar(3) ) THEN
             ljrtar(1) = ie1tar(1)
             ljrtar(2) = ie1tar(2)
        ENDIF
C
        CALL TI_MDIF(ljrtar, lirtar, ldiff, lret)
        ltimes = ldiff/60
        IF ( ltimes .lt. 0 ) good = .false.
C 
        RETURN
        END
