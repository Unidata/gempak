	SUBROUTINE GQSPCL ( szspcl, ispwid, iret )
C************************************************************************
C* GQSPCL								*
C*									*
C* This subroutine gets the special symbol attributes.			*
C*									*
C* GQSPCL ( SZSPCL, ISPWID, IRET )					*
C*									*
C* Output parameters:							*
C*	SZSPCL		REAL		Special symbol size		*
C*	ISPWID		INTEGER		Special symbol line width	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on GQWTHR				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
C------------------------------------------------------------------------
C*	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' ) THEN
	    szspcl = 0
	    ispwid = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    szspcl = ssprsz
	    ispwid = lspwid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
