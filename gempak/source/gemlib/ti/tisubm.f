	SUBROUTINE TI_SUBM  ( idtarr, minuts, jdtarr, iret )
C************************************************************************
C* TI_SUBM								*
C*									*
C* This subroutine subtracts a specified number of minutes from the 	*
C* time in an integer array. The input and output arrays may be the 	*
C* same array.								*
C*									*
C* TI_SUBM  ( IDTARR, MINUTS, JDTARR, IRET )				*
C*									*
C* Input parameters:							*
C*	IDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*	MINUTS		INTEGER		Number of minutes to subtract	*
C*									*
C* Output parameters:							*
C*	JDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* J. Whistler/AWC	 8/95	Based on TI_SUBD			*
C* K. Tyle/GSC		11/95	Fixed leap-year and day/month-change	*
C* M. Linda/GSC		 4/96	Modified syntax so that AIX compiles	*
C* I. Durham/GSC	 9/98	Cleaned up				*
C* S. Jacobs/NCEP	 9/98   Changed minute and hour calculation	*
C* I. Durham/GSC	 9/98	Added correction for negative minutes	*
C* D. Kidwell/NCEP	 4/99	Fixed for Y2K                           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		idtarr (*), jdtarr (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for negative minutes
C
	IF ( minuts .lt. 0 ) THEN
	   iret = -16
	   RETURN
	END IF
C
C*	Get year, month, day, hour, and minute.
C
	iyear  = idtarr (1)
	imonth = idtarr (2)
	iday   = idtarr (3)
	ihour  = idtarr (4)
	iminut = idtarr (5)
C
C*	Get number of days in month.
C
	CALL TI_DAYM ( iyear, imonth, ndays, ier )
C
C*	Check for valid year.
C
	CALL TI_YY24 ( iyear, iyear, iret )
	IF ( iret .lt. 0 )  RETURN
C
C*	Check for valid month.
C
	IF  ( ( imonth .lt. 1 ) .or. ( imonth .gt. 12 ) )  THEN
	    iret = - 8
	    RETURN
	END IF
C
C*	Check for valid day.
C
	IF  ( ( iday .lt. 1 ) .or. ( iday .gt. ndays ) )  THEN
	    iret  = -9
	    RETURN
	END IF
C
C*	Check for first minute of hour.
C
	jmin = MOD ( minuts, 60 )
	jtmp = minuts / 60
	jhr  = MOD ( jtmp, 24 )
	jday = jtmp / 24
C
	iminut = iminut - jmin
	IF ( iminut .lt. 0 ) THEN
	   iminut = iminut + 60
	   jhr = jhr + 1
	END IF
C
C*	Check for first hour of day.
C
	ihour = ihour - jhr
	IF ( ihour .lt. 0 ) THEN
	   ihour = ihour + 24
	   jday = jday + 1
	END IF
C
	DO k = 1, jday
	   IF ( iday .eq. 1 ) THEN
	      imonth = imonth - 1
	      IF ( imonth .eq. 0 ) THEN
		 iyear = iyear - 1
		 imonth = 12
	      END IF
	      CALL TI_DAYM ( iyear, imonth, ndays, ier )
	      iday = ndays
	   ELSE
	      iday = iday - 1
	   END IF
	END DO
C
C*	Put time in output array.
C
	IF ( idtarr ( 1 ) .lt. 100 ) iyear = MOD ( iyear, 100 )
	jdtarr (1) = iyear
	jdtarr (2) = imonth
	jdtarr (3) = iday
	jdtarr (4) = ihour
	jdtarr (5) = iminut
C*
	RETURN
	END
