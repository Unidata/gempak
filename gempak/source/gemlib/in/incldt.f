	SUBROUTINE IN_CLDT  ( cldsym, condtn, iret )
C************************************************************************
C* IN_CLDT								*
C*									*
C* This subroutine decodes the input for the cloud type symbol.		*
C* The input must be in the form:					*
C*			: size : width 					*
C* If the user has entered a condition, it must precede the first 	*
C* colon.								*
C*									*
C* IN_CLDT  ( CLDSYM, CONDTN, IRET )					*
C*									*
C* Input parameters:							*
C*	CLDSYM		CHAR*		Cloud type symbol input		*
C*									*
C* Output parameters:							*
C*	CONDTN		CHAR*		Condition			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* K. Brill/NMC		01/92	Recoded from IN_SKYC after loss		*
C************************************************************************
	CHARACTER*(*)	cldsym, condtn
C*
	REAL		rarr (2)
	CHARACTER	symbol*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for colon.
C
	ipos = INDEX ( cldsym, ':' )
	IF  ( ipos .eq. 0 )  THEN
	    condtn = cldsym
	    RETURN
	  ELSE
	    condtn = cldsym ( 1:ipos-1 )
	    symbol = cldsym ( ipos+1: )
	END IF
C
C*	Decode size, width, and type.
C
	CALL ST_RLST ( symbol, ':', 0. , 2, rarr, num, ier )
	size   = rarr (1)
	iwidth = NINT ( rarr (2) )
	CALL GSCTYP  ( size,  iwidth, ier )
C*
	RETURN
	END
