	SUBROUTINE SS_GSYM  ( symnam, symval, iret )
C************************************************************************
C* SS_GSYM								*
C*									*
C* This subroutine gets the value of a symbol defined on a UNIX system.	*
C*									*
C* SS_GSYM  ( SYMNAM, SYMVAL, IRET )					*
C*									*
C* Input parameters:							*
C*	SYMNAM		CHAR*		Symbol name			*
C*									*
C* Output parameters:							*
C*	SYMVAL		CHAR*		Symbol value			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = symbol not found		*
C** 									*
C* Log:									*
C* D. Burks/CSU		 4/91	UNIX version				*
C************************************************************************
	CHARACTER*(*)	symnam, symval
C*
	CHARACTER	sss*20, s2*20
C------------------------------------------------------------------------
	iret = 0
	CALL ST_LCUC  ( symnam, sss, ier )
	CALL ST_LSTR  ( sss, lsym, ier )
	CALL GETENV   ( sss ( 1 : lsym ), symval )
	IF  ( ( symval (1:1) .eq. ' ' ) .and. ( sss (1:1) .eq. '$' ) )
     +							THEN
	    s2 = sss (2: )
	    lsym = lsym - 1
	    CALL GETENV  ( s2 (1:lsym), symval )
	END IF
	IF  ( symval (1:1) .eq. ' ' )  THEN
	    iret = -1
	END IF
C*
	RETURN
	END
