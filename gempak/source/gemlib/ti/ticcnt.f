	SUBROUTINE TI_CCNT  ( dattim, cent, iret )
C************************************************************************
C* TI_CCNT								*
C*									*
C* This subroutine gets the first 2 digits of a 4-digit year based on   *
C* the standard GEMPAK time.  Any 2-digit year less than or equal to 40 *
C* will be assumed to be in the 21st century; years greater than 40 will*
C* be assumed to be in the 20th century.                                *
C*									*
C* TI_CCNT  ( DATTIM, CENT, IRET )					*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		GEMPAK time			*
C*									*
C* Output parameters:							*
C*	CENT    	CHAR*  		First 2 digits of year          *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C*					 -7 = invalid year		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/99               				*
C* D. Kidwell/NCEP	 4/99 	Allowed 4-digit year; corrected prologue*
C* B. Hebbard/NCEP	 3/18   Moved century break from 2020 to 2040   *
C************************************************************************
	CHARACTER*(*)	dattim, cent
C------------------------------------------------------------------------
	iret = 0
	cent = ' '
C
	CALL ST_LSTR ( dattim, length, ier )
	islash = INDEX ( dattim, '/' )
C
C*	Check for valid dattim string.
C
	IF ( ( length .ge. 11 ) .and. ( islash .eq. 7 ) ) THEN
	    CALL ST_INTG ( dattim ( 1:2 ), iyear, iret )
	    IF ( iret .ne. 0 .or. iyear .lt. 0 ) THEN
	        iret = -7
	      ELSE IF ( iyear .le. 40 ) THEN
	        cent = '20'
	      ELSE
	        cent = '19'
	    END IF
	  ELSE IF ( ( length .ge. 13 ) .and. ( islash .eq. 9 ) ) THEN
	    cent = dattim ( 1:2 )
	  ELSE
	    iret = -1
	END IF
C*
	RETURN
	END
