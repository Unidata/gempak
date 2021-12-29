	SUBROUTINE TI_DTM4  ( dattim, dattm4, iret )
C************************************************************************
C* TI_DTM4								*
C*									*
C* This subroutine modifies a standard GEMPAK time to include a four-   *
C* digit year, instead of a 2-digit year.  Any 2-digit year less than or*
C* equal to 40 will be assumed to be in  the 21st century; years greater*
C* than 40 will be assumed to be in the 20th century.                   *
C*									*
C* TI_DTM4  ( DATTIM, DATTM4, IRET )					*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		GEMPAK time			*
C*									*
C* Output parameters:							*
C*	DATTM4    	CHAR*  		Time with 4-digit year          *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time              *
C*					 -7 = invalid year		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/99               				*
C* D. Kidwell/NCEP	 4/99 	Added 4-digit year check; fixed prologue*
C* B. Hebbard/NCEP       3/18   Moved century break from 2020 to 2040   *
C************************************************************************
	CHARACTER*(*)	dattim, dattm4
C*
	CHARACTER	cent*2
C------------------------------------------------------------------------
	iret   = 0
	dattm4 = ' '
C
C*	Check to see if year is already four digits.
C
	CALL ST_LSTR ( dattim, length, ier )
	islash = INDEX ( dattim, '/' )
C
	IF ( ( length .ge. 13 ) .and. ( islash .eq. 9 ) ) THEN
	    dattm4 = dattim
	  ELSE
	    CALL TI_CCNT ( dattim, cent, iret )
	    IF ( iret .eq. 0 ) THEN
	        dattm4 = cent // dattim
	    END IF
	END IF
C*
	RETURN
	END
