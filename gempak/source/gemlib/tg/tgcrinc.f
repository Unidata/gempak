	SUBROUTINE TG_CRINC ( tbegin, tend, nmin, itftyp, ntime, times, 
     +			      length, iret )
C************************************************************************
C* TG_CRINC								*
C*									*
C* This subroutine returns the times in a time range with an		*
C* increment. This subroutine is intended to be called by C functions.	*
C*									*
C* TG_CRINC  ( TBEGIN, TEND, NMIN, ITFTYP, NTIME, TIMES, LENGTH, IRET )	*
C*									*
C* Input parameters:							*
C*	TBEGIN		CHAR*		Start time			*
C*	TEND		CHAR*		Stop time			*
C*	NMIN		INTEGER		Time increment			*
C*	ITFTYP		INTEGER		Range type			*
C*					  1 = forecast range		*
C*					  2 = date/time range		*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of output times		*
C*	TIMES		CHAR*		Times in range sep by semicolon	*
C*	LENGTH		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-14 = invalid time increment	*
C*					-15 = too many times		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/98						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	tbegin, tend, times
C*
	CHARACTER	tinc*8, ftimes (LLMXGT)*20
C------------------------------------------------------------------------
	iret  = 0
C
C*	Convert the minutes to a character time increment.
C
	ihour = nmin / 60
	imin  = MOD ( nmin, 60 )
	itime = ihour * 100 + imin
	WRITE ( tinc, '(I5.5)' ) itime
C
C*	Get the array of times.
C
	CALL TG_RINC  ( tbegin, tend, tinc, itftyp, ntime, ftimes,
     +			iret )
C
C*	Construct the output string of times from the array.
C
	CALL ST_LSTC  ( ftimes, ntime, ';', times, ier )
	CALL ST_LSTR  ( times, length, ier )
C*
	RETURN
	END
