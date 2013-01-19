	SUBROUTINE IN_PWTH  ( pwth, condtn, iret)
C************************************************************************
C* IN_PWTH 								*
C*									*
C* This subroutine decodes the input for the past weather symbol.  The	*
C* input must be in the form:						*
C*			: size : width					*
C* If the user has entered a condition, it must precede the first :     *
C*									*
C* IN_PWTH  ( PWTH, CONDTN, IRET )					*
C*									*
C* Input parameters:							*
C*	PWTH		CHAR*		Weather symbol input		*
C*									*
C* Output parameters:							*
C*	CONDTN		CHAR*		Condition			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* S. Schotz/GSC	 4/90	GEMPAK5					*
C* M. desJardins/NMC	10/91	Clean up; avoid calls with substrings	*
C* M. desJardins/NMC	11/91	Change * to : and return condition	*
C************************************************************************
	CHARACTER*(*)	pwth, condtn
C*
	REAL		rarr (2)
	CHARACTER	symbol*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for a colon.
C
	ipos = INDEX ( pwth, ':' )
	IF  ( ipos .eq. 0 )  THEN
	    condtn = pwth
	    RETURN
	  ELSE
	    condtn = pwth ( 1:ipos-1 )
	    symbol = pwth ( ipos+1: )
	END IF
C
C*	Decode size and width.
C
	CALL ST_RLST ( symbol, ':', 0. , 2, rarr, num, ier )
	size   = rarr (1)
	iwidth = NINT ( rarr (2) )
	CALL GSPWTH  ( size, iwidth, ier )
C*
	RETURN
	END
