	SUBROUTINE FF_GTIM ( bultin, lenbul, rpttim, iret )
C************************************************************************
C* FF_GTIM								*
C*									*
C* This subroutine will take a plain-language report time string from a	*
C* Flash Flood Guidance bulletin and construct a GEMPAK time string.	*
C*									*
C* The subroutine looks for a line containing a string similar to the 	*
C* following example:							*
C*     "Flash flood guidance issued 1252 AM UTC May 06, 1997"		*
C*									*
C* FF_GTIM ( BULTIN, LENBUL, RPTTIM, IRET )				*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		FFG Bulletin			*
C*	LENBUL 		INTEGER		Bulletin length			*
C*									*
C* Output parameters:							*
C*	RPTTIM		CHAR*		Report date/time string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = cannot decode string	*
C**									*
C* Log:									*
C* K. Tyle/GSC		 5/97						*
C* K. Tyle/GSC		 5/97		Fixed length and hour checks	*
C* D. Kidwell/NCEP      12/97           Changed to process time zones   *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, rpttim
C*
	INTEGER		ltarr(5), iotarr(5)
	LOGICAL		found
	CHARACTER	timstr*48, carr(6)*24, month(12)*3,
     +                  chour*2, cmin*2
	DATA		month /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     +			       'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
C-----------------------------------------------------------------------
	iret  = 0
	DO i  = 1, 6
	    carr (i) = ' '
	END DO
	ixisu = INDEX ( bultin (:lenbul), 'issued ')
	IF ( ixisu .gt. 0 ) THEN
	    iend  = INDEX ( bultin (ixisu:), CHLF )
	    iend  = ixisu + iend - 1
	    ixisu  = ixisu + 7 
	    timstr = bultin (ixisu:iend)
	    CALL ST_CLST ( timstr, ' ', ' ', 6, carr, num, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -1
		RETURN
	    END IF
C
C*	    Build the time.  First string is the hour and minutes.
C
	    CALL ST_LSTR ( carr(1), lens, ier )
	    IF ( lens .eq. 3 ) THEN
		chour = '0' // carr(1)(1:1)
		cmin  = carr(1)(2:3)
	      ELSE IF ( lens .eq. 4 ) THEN
		chour = carr(1)(:2)
		cmin  = carr(1)(3:4)
	      ELSE
		iret = -1
		RETURN
	    END IF
C
	    CALL ST_NUMB ( chour, ihour, ier1 )
	    CALL ST_NUMB ( cmin, imin, ier2 )
	    IF ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) ) THEN
		iret = -1
		RETURN
	    END IF
C
C*	    Second string is AM or PM.
C
	    IF ( ( carr(2) .eq. 'PM' ) .and. ( ihour .lt. 12 ) ) THEN 
		ihour = ihour + 12
	      ELSE IF ( (carr(2) .eq. 'AM') .and. (ihour .eq. 12) ) THEN
		ihour = 0
	    END IF
C
C*	    Fourth string is the month.
C
	    CALL ST_LCUC ( carr(4), carr(4), ier )
	    found = .false.
	    i = 1
	    DO WHILE ( ( i .le. 12 ) .and. ( .not. found ) )
		IF (carr(4)(:3) .eq. month(i)) THEN
		    imonth   = i
		    found = .true.
		  ELSE
		    i = i + 1
		END IF
	    END DO
	    IF ( .not. found ) THEN
		iret = -1
		RETURN
	    END IF
C
C*	    Fifth string is the date.
C
	    CALL ST_LSTR ( carr(5), lens , ier )
	    IF ( lens .ne. 3 ) THEN
		iret = -1
		RETURN
	    END IF
	    CALL ST_NUMB ( carr(5)(:2), iday, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -1
		RETURN
	    END IF
C
C*	    Sixth string is the year.
C		
	    CALL ST_LSTR ( carr(6), lens , ier )
	    IF ( lens .ne. 7 ) THEN
		iret = -1 
		RETURN
	    END IF
	    CALL ST_NUMB ( carr(6)(:4), iyear, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -1
		RETURN
	    END IF
C
C*	    Construct the report time.
C
	    ltarr (1) = iyear
	    ltarr (2) = imonth
	    ltarr (3) = iday
	    ltarr (4) = ihour
	    ltarr (5) = imin
C
C*	    Convert to GMT.  Third string is the time zone.
C
     	    CALL TI_TZDF ( ltarr, carr(3), 'GMT', iotarr, tdiff, ier ) 
     	    IF ( ier .eq. 0 ) THEN
 		CALL TI_ITOC ( iotarr, rpttim, ier )
 		IF ( ier .lt. 0 ) iret = -1
       	      ELSE
 		iret = -1
     	    END IF
	END IF
C*
	RETURN
	END
