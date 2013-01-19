	SUBROUTINE IN_NUMB  ( string, hdef, iwdef,
     +			      condtn, hght, iwide, iret )
C************************************************************************
C* IN_NUMB								*
C*									*
C* This subroutine decodes the input for the numerical weather		*
C* parameters.  The input must be in the form:				*
C*									*
C*		: hght : iwide						*
C*			-OR-						*
C*		cond : hght : iwide					*
C*									*
C* If the user has entered a condition, it must precede the first :	*
C*									*
C* IN_NUMB  ( STRING, HDEF, IWDEF, CONDTN, HGHT, IWIDE, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Weather parameter		*
C*	HDEF		REAL		Default text size (height)	*
C*	IWDEF		INTEGER		Default line width		*
C*									*
C* Output parameters:							*
C*	CONDTN		CHAR*		Remaining condition		*
C*	HGHT		REAL		Text size			*
C*	IWIDE		INTEGER		Text width			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* R. Jones/NCEP	 5/06	Original version			*
C* R. Jones/NCEP	 7/06	Changed order of vars in call seq	*
C************************************************************************
	CHARACTER*(*)	string, condtn
C*
	CHARACTER	rest*24, carr(2)*24
C------------------------------------------------------------------------
C*
	iret = 0
C
C*	Check for a colon.
C
	ipos = INDEX ( string, ':' )
	IF  ( ipos .eq. 0 )  THEN
C
C*	    If not found set the return values and return.
C
	    condtn = string
	    hght   = hdef
	    iwide  = iwdef
	    RETURN
	  ELSE
C
C*	    If found, split the input at the first colon.
C
	    condtn = string ( 1:ipos-1 )
	    rest   = string ( ipos+1: )
	END IF
C
C*	Decode size and width.
C
	CALL ST_CLST ( rest, ':', ' ', 2, carr, numc, ier )
C
C*	Process the text size.
C
	hght = hdef
	IF  ( carr(1) .ne. ' ' )  THEN
C
C*	    Try a regular floating point value first.
C
	    CALL ST_CRNM ( carr(1), tsiz, ier )
	    IF  ( ier .eq. 0 )  THEN
		hght = tsiz
	    ELSE
C
C*		If that failed, try a font size name. Allow for a
C*		negative sign before the name.
C
		is   = 1
		ichr = 1
		IF  ( carr(1) ( 1:1 ) .eq. '-' )  THEN
		    is   = -1
		    ichr = 2
		END IF
		CALL TB_FONT ( carr(1)(ichr:), tsiz, ier )
		IF  ( ier .eq. 0 )  THEN
		    hght = tsiz * is
		END IF
	    END IF
	END IF
C
C*	Process the text width. Negative values are not allowed.
C
	iwide = iwdef
	IF  ( carr(2) .ne. ' ' )  THEN
	    CALL ST_NUMB ( carr(2), iw, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  ( iw .gt. 0 )  THEN
		    iwide = iw
		END IF
	    END IF
	END IF
C
C
	RETURN
	END
