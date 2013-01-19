	SUBROUTINE IN_SKYC  ( skysym, condtn, iret )
C************************************************************************
C* IN_SKYC								*
C*									*
C* This subroutine decodes the input for the sky coverage symbol.	*
C* The input must be in the form:					*
C*			: size : width : type				*
C* If the user has entered a condition, it must precede the first :	*
C*									*
C* IN_SKYC  ( SKYSYM, CONDTN, IRET )					*
C*									*
C* Input parameters:							*
C*	SKYSYM		CHAR*		Sky coverage symbol input	*
C*									*
C* Output parameters:							*
C*	CONDTN		CHAR*		Condition			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* S. Schotz/GSC	 4/90	GEMPAK5					*
C* S. Schotz/GSC	 7/90	Changed order of parts			*
C* M. desJardins/NMC	10/91	Clean up; elim calls with substrings	*
C* M. desJardins/NMC	11/91	Change * to : and retufn condition	*
C************************************************************************
	CHARACTER*(*)	skysym, condtn
C*
	REAL		rarr (3)
	CHARACTER	symbol*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for colon.
C
	ipos = INDEX ( skysym, ':' )
	IF  ( ipos .eq. 0 )  THEN
	    condtn = skysym
	    RETURN
	  ELSE
	    condtn = skysym ( 1:ipos-1 )
	    symbol = skysym ( ipos+1: )
	END IF
C
C*	Decode size, width, and type.
C
	CALL ST_RLST ( symbol, ':', 0. , 3, rarr, num, ier )
	size   = rarr (1)
	iwidth = NINT ( rarr (2) )
	itype  = rarr (3)
	CALL GSSKY  ( size, itype, iwidth, ier )
C*
	RETURN
	END
