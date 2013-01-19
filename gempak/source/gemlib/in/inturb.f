	SUBROUTINE IN_TURB  ( tbsy, condtn, iret)
C************************************************************************
C* IN_TURB								*
C*									*
C* This subroutine decodes the input for the turbulence symbol.  The	*
C* input must be in the form:						*
C*			: size : width					*
C* If the user has entered a condition, it must precede the first :     *
C*									*
C* IN_TURB  ( TBSY, CONDTN, IRET )					*
C*									*
C* Input parameters:							*
C*	TBSY		CHAR*		Turbulence symbol input		*
C*									*
C* Output parameters:							*
C*	CONDTN		CHAR*		Condition			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		10/99						*
C************************************************************************
	CHARACTER*(*)	tbsy, condtn
C*
	REAL		rarr (2)
	CHARACTER	symbol*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for a colon.
C
	ipos = INDEX ( tbsy, ':' )
	IF  ( ipos .eq. 0 )  THEN
	    condtn = tbsy
	    RETURN
	  ELSE
	    condtn = tbsy ( 1:ipos-1 )
	    symbol = tbsy ( ipos+1: )
	END IF
C
C*	Decode size and width.
C
	CALL ST_RLST ( symbol, ':', 0. , 2, rarr, num, ier )
	size   = rarr (1)
	iwidth = NINT ( rarr (2) )
	CALL GSTURB  ( size, iwidth, ier )
C*
	RETURN
	END
