	SUBROUTINE IN_WSYM  ( wsym, condtn, iret)
C************************************************************************
C* IN_WSYM								*
C*									*
C* This subroutine decodes the input for the weather symbol.  The	*
C* input must be in the form:						*
C*			: size : width					*
C* If the user has entered a condition, it must precede the first :     *
C*									*
C* IN_WSYM  ( WSYM, CONDTN, IRET )					*
C*									*
C* Input parameters:							*
C*	WSYM		CHAR*		Weather symbol input		*
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
	CHARACTER*(*)	wsym, condtn
C*
	REAL		rarr (2)
	CHARACTER	symbol*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for a colon.
C
	ipos = INDEX ( wsym, ':' )
	IF  ( ipos .eq. 0 )  THEN
	    condtn = wsym
	    RETURN
	  ELSE
	    condtn = wsym ( 1:ipos-1 )
	    symbol = wsym ( ipos+1: )
	END IF
C
C*	Decode size and width.
C
	CALL ST_RLST ( symbol, ':', 0. , 2, rarr, num, ier )
	size   = rarr (1)
	iwidth = NINT ( rarr (2) )
	CALL GSWTHR  ( size, iwidth, ier )
C*
	RETURN
	END
