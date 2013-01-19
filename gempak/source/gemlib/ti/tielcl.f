	SUBROUTINE TI_ELCL ( dattim, zone, noonmid, timlcl, iret )
C************************************************************************
C* TI_ELCL								*
C*									*
C* This subroutine converts a GEMPAK time to a plain language local     *
C* date/time string. The local time zone may be explicitly set or the   *
C* system time zone may be used as the default. The plain language 	*
C* string will be in the form of:  					*
C*     HHMM AA ZZZ WWW MMM DD YYYY                                      *
C* where								*
C*     HHMM	hour and minutes of local time ( 1 <= HH <= 12 )        *
C*     AA	AM or PM                                                *
C*     ZZZ	local time zone                                         *
C*     WWW	day of the week 3 letter abbreviation                   *
C*     MMM      Month 3 letter abbreviation                             *
C*     DD	day of the month (1 <= DD <= 31 )                       *
C*     YYYY	4 digit year                                            *
C*									*
C* Example:                                                             *
C*      917 AM EDT THU JUL 25 2002                                      *
C*									*
C* TI_ELCL ( DATTIM, ZONE, TIMLCL, IRET )                               *
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		GEMPAK time  (YYMMDD/HHNN)   	*
C*      ZONE		CHAR*		Local time zone - 3 char 	*
C*      NOONMID		LOGICAL		Flag to use NOON or MIDNIGHT 	*
C*									*
C* Output parameters:							*
C*	TIMLCL		CHAR*		Local date/time string          *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid dattim string  	*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 6/03						*
C* A. Hardy/NCEP	 9/03		Added time zone option to call	*
C* B. Yin/SAIC           3/04   	Changed SS_GTIM to CSS_GTIM     *
C* m.gamazaychikov/SAIC	 3/06		Added noonmid flag to call	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	timlcl, dattim, zone
        LOGICAL         noonmid
C*
	INTEGER		iotarr (5), jarr (5), iwkdy, itime
	CHARACTER	datstr*60, month (12)*3, timdes*10,
     +                  chour*3, chmin*3, cday*3,
     +                  dwname (7)*4, pmm*4, pdwk*4, cyear*5,
     +			ampm*3, cdttm*20, syszon*4
C
	DATA		month /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     +			       'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
	DATA		dwname /'SUN', 'MON', 'TUE', 'WED', 'THU',
     +				'FRI', 'SAT'/
C-----------------------------------------------------------------------
	timlcl = ' ' 
        pmm = ' '
        pdwk = ' '
C
	CALL ST_LSTR ( dattim, lens, ier )
	datstr = dattim ( :lens )
C
C*      Fill in missing parts of dattim string.
C
        IF ( ( lens .lt. 11 ) .or. ( lens .gt. 13 ) ) THEN
	    itime = 1
            CALL CSS_GTIM ( itime, cdttm, ier )
            CALL TI_STAN ( dattim, cdttm, datstr, ier )
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'TI', ier, dattim, ierr )
                iret = ier
                return
            ENDIF
        END IF
C
C*      Convert the GEMPAK time into an integer array.
C
	CALL TI_CTOI ( datstr, iotarr, ier )
C
C*      Find the local time zone from the system time.
C
        itype = 0
        CALL CSS_DATE ( itype, iyear, imon, iday, ihour, imin,
     +			   isec, julian, syszon, ier )
C
C*	Use time zone from system time.
C
        IF ( zone .eq. ' ' ) THEN
            zone = syszon
        END IF
C
C*      Convert time to local time.
C
	CALL TI_TZDF ( iotarr, 'UTC', zone, jarr, hdiff, ier )
C
C*      Set the 'AM' or 'PM' designator and local hour.
C
        IF ( jarr(4) .eq. 12 ) THEN
            newtime = jarr(4)
            ampm = 'PM'
          ELSE IF ( jarr(4) .eq. 0 ) THEN
            newtime = 12
            ampm = 'AM'
          ELSE IF (( jarr(4) .ge. 1) .and. (jarr(4) .le. 11)) THEN
            newtime = jarr(4)
            ampm = 'AM'
          ELSE IF ( jarr(4) .gt. 12 ) THEN
            newtime = jarr(4) - 12
            ampm = 'PM'
        END IF
C
C*      Get the number of the day of the week.
C
	CALL TI_DAYW( jarr, iwkdy, ier )
C
C*      Get the 3 char. abbreviations for the month.
C
        IF ( (jarr(2) .ge. 1) .and. (jarr(2) .le. 12 ) ) THEN
            pmm = month ( jarr(2) )
        END IF
C
C*      Get the 3 char. abbreviation for the day of the week.
C
        IF ( (iwkdy .ge. 1) .and. (iwkdy .le. 7 ) ) THEN
            pdwk = dwname( iwkdy )
        END IF
C
C*	Create local time string.
C
        CALL ST_INCH ( jarr(1), cyear, ier )
        CALL ST_INCH ( jarr(3), cday, ier )
        CALL ST_INCH ( newtime, chour, ier )
        CALL ST_INCH ( jarr(5), chmin, ier )
C
        CALL ST_LSTR ( chour, lenh, ier )
        CALL ST_LSTR ( cday, lend, ier )
        CALL ST_LSTR ( chmin, lenm, ier )
        CALL ST_LSTR ( zone, lenz, ier )
        IF ( lenm .eq. 1 ) THEN
            chmin = '0' // chmin
        END IF
       
C
C*      Check if local hour is NOON or MIDNIGHT.
C
        IF ( noonmid .and. 
     +       ( ( newtime .eq. 12 ) .and. ( jarr(5) .eq. 0 ) ) ) THEN 
                 IF (  ampm .eq. 'AM' ) THEN
                    timdes = 'MIDNIGHT ' 
                   ELSE IF (  ampm .eq. 'PM' ) THEN
                    timdes = 'NOON ' 
                 END IF
          ELSE
	    timdes = chour(:lenh) // chmin(:2) // ' ' // ampm // ' '
        END IF
        CALL ST_LSTR ( timdes, lent, ier )
C
C*      Create the local time string.
C
        timlcl = timdes(:lent) // ' ' // zone(:lenz) // ' ' // pdwk(:3) 
     +           // ' '  // pmm(:3) // ' '  // cday(:lend) // ' ' 
     +          // cyear
C*
	RETURN
        END
