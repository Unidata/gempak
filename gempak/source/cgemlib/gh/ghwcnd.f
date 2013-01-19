	SUBROUTINE GH_WCND  ( condtn, size, iwidth, iret )
C************************************************************************
C* GH_WCND								*
C*									*
C* This subroutine gets the size from the condition string in SFGRAM.	*
C* The condition string must be in the form *size*iwidth.		*
C*									*
C* GH_WCND  ( CONDTN, SIZE, IWIDTH, IRET )				*
C*									*
C* Input parameters:							*
C*	CONDTN		CHAR*		Condition string		*
C*									*
C* Output parameters:							*
C*	SIZE		REAL		Symbol size			*
C*	IWIDTH		INTEGER		Symbol line width		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Copied from SFXCND			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	condtn
C*
	CHARACTER	carr (3)*8, ccc*8
C------------------------------------------------------------------------
	iret = 0
C
C*	Break the string into two parts.
C
	CALL ST_CLST  ( condtn, ':', ' ', 3, carr, n, ier )
C
C*	Get the size from the first part.
C
	IF  ( carr (2) .eq. ' ' )  THEN
	    size = 0.
	  ELSE
	    ccc  = carr (2)
	    CALL ST_CRNM  ( ccc, size, ier )
	    IF  ( ier .ne. 0 )  size = 0.
	END IF
C
C*	Get the width from the second part.
C
	IF  ( carr (3) .eq. ' ' )  THEN
	    iwidth = 0
	  ELSE
	    CALL ST_NUMB  ( carr (3), iwidth, ier )
	    IF  ( ier .ne. 0 )  iwidth = 0
	END IF
C*
	RETURN
	END
