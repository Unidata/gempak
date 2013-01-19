	SUBROUTINE GQICNG ( szicng, icewid, iret )
C************************************************************************
C* GQICNG								*
C*									*
C* This subroutine gets the icing symbol attributes.			*
C*									*
C* GQICNG ( SZICNG, ICEWID, IRET )					*
C*									*
C* Output parameters:							*
C*	SZICNG		REAL		Icing symbol size		*
C*	ICEWID		INTEGER		Icing symbol line width		*
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
	    szicng = 0
	    icewid = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    szicng = scersz
	    icewid = lcewid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
