	SUBROUTINE TI_LOCL ( timlcl, dattim, iret )
C************************************************************************
C* TI_LOCL								*
C*									*
C* This subroutine converts a plain language local date/time string to  *
C* a GEMPAK time string.  The plain language string must be of the form *
C*     HHMM AA ZZZ WWW MMM DD YYYY                                      *
C* where								*
C*     HHMM	hour and minutes of local time ( 1 <= HH <= 12 )        *
C*     AA	AM or PM                                                *
C*     ZZZ	local time zone                                         *
C*     WWW	day of the week 3 letter abbreviation                   *
C*     MMM      Month 3 letter abbreviation                             *
C*     DD	day of the month (1 <= DD <= 31 )                       *
C*     YYYY	4 digit year                                            *
C* Alphabetic data may be upper or lower case.                          *
C* Example:                                                             *
C*      917 AM EDT THU JUL 25 2002                                      *
C*									*
C* TI_LOCL ( TIMLCL, DATTIM, IRET )                                     *
C*									*
C* Input parameters:							*
C*	TIMLCL		CHAR*		Local date/time string          *
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK time     		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid year              *
C*					 -8 = invalid month             *
C*					 -9 = invalid day               *
C*					-10 = invalid hour              *
C*					-11 = invalid minute            *
C*					-17 = invalid local d/t string  *
C**									*
C* Log:									*
C* D. Kidwell/NCEP       7/02 	From FF_GTIM                            *
C* M. Li/SAIC		10/02	Check and remove substring in ( )	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	timlcl, dattim
C*
	INTEGER		ltarr (5), iotarr (5)
	LOGICAL		found
	CHARACTER	timstr*60, tmp*60, carr (7)*6, month (12)*3,
     +                  chour*2, cmin*2, cday*2, tmptim*20
	DATA		month /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     +			       'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
C-----------------------------------------------------------------------
	iret   = -17
	dattim = ' ' 
	CALL ST_LSTR ( timlcl, lens, ier )
	timstr = timlcl ( :lens )
	CALL ST_LCUC ( timstr, timstr, ier )
C
C*	Check for and remove the substring enclosed in parentheses
C
	ip1 = INDEX ( timstr, '(' )
	IF ( ip1 .gt. 0 ) THEN
	    ip2 = INDEX ( timstr, ')' )
	    IF ( ip2 .gt. ip1 ) THEN
	       CALL ST_RMST (timstr, timstr(ip1:ip2), ipos, tmp, ier)
	       CALL ST_LSTR ( tmp, lens, ier )
	       timstr = tmp ( :lens )
	    END IF 
	END IF
C
	CALL ST_CLST ( timstr, ' ', ' ', 7, carr, num, ier )
	IF ( ( ier .ne. 0 ) .or. ( num .lt. 7 ) )  RETURN
C
C*	Build the time.  First string is the hour and minutes.
C
	CALL ST_LSTR ( carr ( 1 ), lens, ier )
	IF ( ( lens .eq. 3 ) .or. ( lens .eq. 4 ) ) THEN
	    chour = carr ( 1 ) ( :lens - 2 )
	    cmin  = carr ( 1 ) ( lens - 1:lens )
	  ELSE
	    RETURN
 	END IF
C
	CALL ST_NUMB ( chour, ihour, ier1 )
	CALL ST_NUMB ( cmin, imin, ier2 )
	IF ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) ) THEN
	    IF ( ier1 .ne. 0 ) THEN
		iret = -10
	      ELSE
	        iret = -11
	    END IF
	    RETURN
	END IF
C
C*	Second string is AM or PM.
C
	IF ( ( carr (2) .eq. 'AM' ) .or. ( carr (2) .eq. 'PM' ) ) THEN
	    IF ( ( carr(2) .eq. 'PM' ) .and. ( ihour .lt. 12 ) ) THEN 
		ihour = ihour + 12
	      ELSE IF ( (carr(2) .eq. 'AM') .and. (ihour .eq. 12) ) THEN
		ihour = 0
	    END IF
	  ELSE
	    RETURN
	END IF
C
C*	Fifth string is the month.
C
	found = .false.
	ii    = 1
	DO WHILE ( ( ii .le. 12 ) .and. ( .not. found ) )
	    IF ( carr ( 5 ) ( :3 ) .eq. month ( ii ) ) THEN
	        imonth = ii
	        found  = .true.
	      ELSE
	        ii     = ii + 1
	    END IF
	END DO
	IF ( .not. found ) THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Sixth string is the date.
C
	CALL ST_LSTR ( carr ( 6 ), lens , ier )
	IF ( ( lens .eq. 1 ) .or. ( lens .eq. 2 ) ) THEN
	    cday = carr ( 6 ) ( :lens )
	    CALL ST_NUMB ( cday, iday, ier )
	    IF ( ier .ne. 0 ) iret = -9
	  ELSE
	    iret = -9
	END IF
	IF ( iret .eq. -9 )  RETURN
C
C*	Seventh string is the year.
C		
	CALL ST_LSTR ( carr ( 7 ), lens , ier )
	IF ( lens .eq. 4 ) THEN
	    CALL ST_NUMB ( carr ( 7 )( :4 ), iyear, ier )
	    IF ( ier .ne. 0 ) iret = -7
	  ELSE
	    iret = -7 
	END IF
	IF ( iret .eq. -7 )  RETURN
C
C*	Construct the report time.
C
	ltarr (1) = iyear
	ltarr (2) = imonth
	ltarr (3) = iday
	ltarr (4) = ihour
	ltarr (5) = imin
C
C*	Check that each of these numeric values is within range.
C
	CALL TI_ITOC ( ltarr, tmptim, iret )
	IF ( iret .ne. 0 )  RETURN
C
C*	Convert to UTC.  Third string is the local time zone.
C
     	CALL TI_TZDF ( ltarr, carr ( 3 ), 'UTC', iotarr, tdiff, iret ) 
     	IF ( iret .eq. 0 )  CALL TI_ITOC ( iotarr, dattim, iret )
C*
	RETURN
	END
