	SUBROUTINE IS_TIME ( report, lenr, ptime, iptr, iret )
C************************************************************************
C* IS_TIME 								*
C*									*
C* This subroutine decodes a UTC time (hours and minutes) from an       *
C* international sigmet report.                                         *
C*                                                                      *
C* IS_TIME ( REPORT, LENR, PTIME, IPTR, IRET )                          *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*									*
C* Output parameters:							*
C*	PTIME		CHAR*  		Time field hhmm (UTC)           *
C*	IPTR		INTEGER		Pointer to location after UTC   *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-11 = valid time not found      *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, ptime
C*
	CHARACTER	carr (10)*6
C------------------------------------------------------------------------
	iret  = 0
	iptr  = 0
	ptime = ' '
C
C*	Look for UTC.
C
	len  = MIN ( 20, lenr ) 
	CALL ST_CLST ( report ( :len ), ' ', ' ', 10, carr, num, ier )
	DO i = 2, num
	    IF ( carr ( i ) ( :3 ) .eq. 'UTC' ) THEN
	        CALL ST_LSTR ( carr ( i - 1 ), lens, ier )
	        IF ( lens .ge. 4 ) THEN
	            ptime = carr ( i - 1 ) ( lens - 3:lens )
	            iptr  = INDEX ( report ( :len ), 'UTC' ) + 3
	            RETURN
	        END IF
	    END IF
	END DO
	iret = -11
C*
	RETURN
	END
