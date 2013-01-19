	SUBROUTINE GQSKY  ( szsky, isktyp, iskwid, iret)
C************************************************************************
C* GQSKY								*
C*									*
C* This subroutine returns the current sky coverage symbol size, line	*
C* width, and sky symbol type.						*
C*									*
C* Note that this subroutine is not currently implemented.		*
C*									*
C* GQSKY  ( SZSKY, ISKTYP, ISKWID, IRET )				*
C*									*
C* Output parameters:							*
C*	SZSKY		REAL		Sky coverage size multiplier	*
C*	ISKTYP		INTEGER		Sky coverage symbol type	*
C*					  1 = not filled in		*
C*					  2 = filled in			*
C*	ISKWID		INTEGER		Sky coverage line width		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szsky = 0
	    isktyp = 0
	    iskwid = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
            szsky = sskysz
	    isktyp = lsktyp
	    iskwid = lskwid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
