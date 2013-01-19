	SUBROUTINE GH_TIME ( timstr, tzone, jtarr, hours, zone2,
     +                       fmonth, fmon, fday, ampm, iret )
C************************************************************************
C* GH_TIME								*
C*									*
C* This subroutine creates the time information for the label box	*
C*									*
C* GH_TIME ( TIMSTR, TZONE, JTARR, HOURS, ZONE2, FMONTH, FMON, FDAY,    *
C*	     AMPM, IRET ) 					        *
C*									*
C* Input parameters:							*
C*	TIMSTR		CHAR*		Valid time for advisory		*
C*	TZONE		CHAR*		Time zone character		*
C*									*
C* Output parameters:							*
C*	JTARR(*)	INTEGER		Time array (YYYY,MM,DD,HH,MM )  *
C*	HOURS		REAL		Hours diff. between zones	*
C*	ZONE2		CHAR*		Local time zone 		*
C*   	FMONTH		CHAR*		Current full month		*
C*   	FMON  		CHAR*		Current abbreviated month	*
C*	FDAY		CHAR*		Current day of the week         *
C*   	AMPM		CHAR*		Current time of day designation *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01   					*
C* m.gamazaychikov/SAIC	06/06	Added HST for zone2			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	timstr, tzone, zone2, ampm, fmonth, fmon, fday
        INTEGER         jtarr(*)
        REAL            hours
C*
	CHARACTER	mon(12)*3, month(12)*9, day(7)*9, zone1*3, sd*1
        INTEGER		itarr(5)
        LOGICAL         dst
C*
	DATA		mon /   'JAN', 'FEB', 'MAR', 'APR',
     +                          'MAY', 'JUN', 'JUL', 'AUG', 
     +                          'SEP', 'OCT', 'NOV', 
     +                          'DEC' /
	DATA		month / 'January', 'February', 'March', 'April',
     +                          'May', 'June', 'July', 'August', 
     +                          'September', 'October','November', 
     +                          'December' /
	DATA		day /   'Sunday', 'Monday', 'Tuesday', 
     +                          'Wednesday', 'Thursday', 'Friday', 
     +                          'Saturday' /
C-----------------------------------------------------------------------
	iret = 0
        zone1 = 'UTC'
C
C*	Find the month and year string.
C
	CALL TI_CTOI ( timstr, itarr, iret )
        IF ( iret .ne. 0 )  THEN
            RETURN
        END IF
C
C*      Check for standard time.
C
        CALL TI_DST ( itarr, dst, iret )
        IF ( dst ) THEN
            sd = 'D'
          ELSE
            sd = 'S'
        END IF
        CALL ST_LSTR ( tzone, lenz, iret )
C
        zone2 = tzone (:lenz) // sd // 'T'
        IF ( tzone .eq. 'H' ) zone2 = 'HST'
        CALL ST_LSTR ( zone2, lenz, iret )
C
C*	Find the local time.
C
	CALL TI_TZDF ( itarr, zone1, zone2 (:lenz), jtarr, hours, iret )
C
C*	Find the day of the week.
C
        CALL TI_DAYW ( jtarr, iday, iret )
        IF ( iret .ne. 0 )  THEN
            RETURN
        END IF
        fday = day ( iday )
C
C*      Find the month and abbreviated month.
C
        IF ( ( jtarr (2) .ge. 1) .and. (jtarr (2) .le. 12 ) ) THEN
            fmon = mon ( jtarr (2) ) 
            fmonth = month ( jtarr (2) ) 
        END IF
C
C*	Determine if a.m. or p.m. and adjust to 12-hour clock.
C
	IF ( jtarr (4) .ge. 12 ) THEN
            ampm = 'PM'
	    IF ( jtarr (4) .gt. 12 ) jtarr (4) = jtarr (4) - 12
	  ELSE
	    ampm = 'AM'
	    IF ( jtarr (4) .eq. 0 ) jtarr (4) = 12
        END IF
C*
	RETURN
	END
