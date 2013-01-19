	SUBROUTINE TI_JTOI  ( jyear, jday, idtarr, iret )
C************************************************************************
C* TI_JTOI								*
C*									*
C* This subroutine converts from the day of the year to a date array.	*
C* Note that this routine returns only 3 values, namely year, month	*
C* and day.								*
C*									*
C* TI_JTOI  ( JYEAR, JDAY, IDTARR, IRET )				*
C*									*
C* Input parameters:							*
C*	JYEAR		INTEGER		Year				*
C*	JDAY		INTEGER		Day of the year			*
C*									*
C* Output parameters:							*
C*	IDTARR (3)	INTEGER		Date array (YYYY,MM,DD)		*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid year		*
C*					 -9 = invalid day		*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 9/94						*
C* P. Bruehl/Unidata	12/94		Fixed bug for December		*
C* S. Jacobs/NMC	12/94		Fixed check for leap year	*
C* I. Durham/GSC         9/98		Cleaned up			*
C* D. Kidwell/NCEP       2/99		Corrected return code, 7 -> -7  *
C* D. Kidwell/NCEP       4/99		Changed to use TI_YY24          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	INTEGER		idtarr (*)
C*
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for valid day.
C
	IF  ( ( jday .lt. 0 ) .or. ( jday .gt. 366 ) )  THEN
	    iret  = -9
	    RETURN
	END IF
C
C*	Check for valid year. Make sure that the year includes the
C*	century.
C
	CALL TI_YY24 ( jyear, iyear, iret )
	IF ( iret .lt. 0 )  RETURN 
C
C*	Find the date by stepping through the months and subtracting
C*	the number of days in the month, from the day of the year.
C
	i = 1
	iday = jday
	done = .false.
	DO  WHILE ( ( i .le. 12 ) .and. ( .not. done ) )
	    CALL TI_DAYM ( iyear, i, ndays, ier )
	    IF ( iday .gt. ndays ) THEN
	       IF ( i .eq. 12 ) THEN
		  iret = -9
		  RETURN
	       ELSE
		  iday = iday-ndays
		  i=i+1
               END IF
	    ELSE
		imonth = i
		done =.true.
	    END IF
	END DO
C
C*	Put time in output array.
C
	idtarr (1) = iyear
	idtarr (2) = imonth
	idtarr (3) = iday
C*
	RETURN
	END
