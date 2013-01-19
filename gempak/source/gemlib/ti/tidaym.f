	SUBROUTINE TI_DAYM  ( iyear, imon, iday, iret )
C************************************************************************
C* TI_DAYM								*
C*									*
C* This subroutine returns the number of days in the given month.	*
C* The year must be a full four-digit year.				*
C*									*
C* TI_DAYM  ( IYEAR, IMON, IDAY, IRET )					*
C*									*
C* Input parameters:							*
C*	IYEAR 		INTEGER		Year (YYYY)			*
C*	IMON		INTEGER		Month (MM)			*
C*									*
C* Output parameters:							*
C*	IDAY		INTEGER		Number of days in the month	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid month		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 4/96						*
C* I. Durham/GSC	 9/98	Added comment line			*
C************************************************************************
	INTEGER 	month (12)
	LOGICAL		LEAP
C*
	LEAP (iyr) =	( ( MOD ( iyr, 4 ) .eq. 0 ) .and.
     +			  ( ( MOD ( iyr, 100 ) .ne. 0 ) .or.
     +			    ( MOD ( iyr, 400 ) .eq. 0 ) ) )
C*
	DATA		month / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31,
     +				30, 31 / 
C------------------------------------------------------------------------
	IF ( imon .gt. 0 .and. imon .lt. 13 ) THEN
C
C*	Pick the number of days for the given month.
C
	    iday = month ( imon )
	    IF  ( ( imon .eq. 2 ) .and. LEAP ( iyear ) )  iday = iday + 1
	    iret = 0
	ELSE
	    print *, "TI_DAYM:  WARNING:  Invalid month = ", imon
	    iret = -1
	END IF
C*
	RETURN
	END
