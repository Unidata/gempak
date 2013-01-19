	SUBROUTINE RU_TEMP  ( field, t, td, iret )
C************************************************************************
C* RU_TEMP								*
C*									*
C* This subroutine decodes a temperature/dewpoint field.  The field	*
C* is encoded as TTtDD where TT is temperature in degrees Celsius,	*
C* t is approximate tenths of degree and sign indicator, and DD is	*
C* the dewpoint depression in degrees Celsius.				*
C*									*
C* RU_TEMP ( FIELD, T, TD, IRET )					*
C*									*
C* Input parameters:							*
C*	FIELD		CHAR*		Encoded group			*
C*									*
C* Output parameters:							*
C*	T		REAL		Temperature			*
C*	TD		REAL		Dewpoint temperature		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	field
C------------------------------------------------------------------------
	iret = 0
C*
	t  = RMISSD
	td = RMISSD
C
C*	The first three digits of the field contain encoded temperature.
C*	If the temperature is missing (as indicated by a /) then the
C*	conversion to integer will fail.
C
	CALL ST_INTG  ( field (1:3), itemp, ier )
	IF  ( ier .eq. 0 )  THEN
C
C*	    If the integer is even, the temperature is positive.  Otherwise,
C*	    the temperature is negative.
C
	    isign = MOD ( itemp, 2 )
	    IF  ( isign .eq. 1 )  itemp = - itemp
	    t = FLOAT ( itemp ) / 10.
C
C*	    The last two digits contain the encoded dewpoint depression.
C
	    CALL ST_INTG ( field (4:5), idep, ier )
C
C*	    Check that the dewpoint value is not missing.
C
	    IF ( ier .eq. 0 ) THEN
C
C*		If the dewpoint given is less than 50, the dewpoint is
C*		given in tenths of a degree.  For numbers greater than
C*		55, the dewpoint is given in whole degrees plus 50.
C*		The values 51 - 55 are not used.
C
		IF  ( idep .le. 50 )  THEN
		    ddep = FLOAT ( idep ) / 10.
		  ELSE
		    ddep = FLOAT ( idep - 50 )
		END IF
		td = t - ddep
	    END IF
	END IF
C*
	RETURN
	END
