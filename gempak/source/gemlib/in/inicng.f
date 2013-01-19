	SUBROUTINE IN_ICNG  ( icsy, condtn, iret)
C************************************************************************
C* IN_ICNG								*
C*									*
C* This subroutine decodes the input for the icing symbol.  The		*
C* input must be in the form:						*
C*			: size : width					*
C* If the user has entered a condition, it must precede the first :     *
C*									*
C* IN_ICNG  ( ICSY, CONDTN, IRET )					*
C*									*
C* Input parameters:							*
C*	ICSY		CHAR*		Icing symbol input		*
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
	CHARACTER*(*)	icsy, condtn
C*
	REAL		rarr (2)
	CHARACTER	symbol*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for a colon.
C
	ipos = INDEX ( icsy, ':' )
	IF  ( ipos .eq. 0 )  THEN
	    condtn = icsy
	    RETURN
	  ELSE
	    condtn = icsy ( 1:ipos-1 )
	    symbol = icsy ( ipos+1: )
	END IF
C
C*	Decode size and width.
C
	CALL ST_RLST ( symbol, ':', 0. , 2, rarr, num, ier )
	size   = rarr (1)
	iwidth = NINT ( rarr (2) )
	CALL GSICNG  ( size, iwidth, ier )
C*
	RETURN
	END
