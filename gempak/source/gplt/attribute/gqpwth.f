	SUBROUTINE GQPWTH  ( szpwth, ipwwid, iret)
C************************************************************************
C* GQPWTH								*
C*									*
C* This subroutine returns the past weather symbol size and		*
C* line width.								*
C*									*
C* Note that this subroutine is not currently implemented.		*
C*									*
C* GQPWTH  ( SZPWTH, IPWWID, IRET )					*
C*									*
C* Output parameters:							*
C*	SZPWTH		REAL		Past weather symbol size	*
C*	IPWWID		INTEGER		Past weather symbol width	*
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
	    szpwth = 0
	    ipwwid = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
            szpwth = spwtsz
	    ipwwid = lpwwid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
