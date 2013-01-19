	SUBROUTINE GQTURB ( szturb, ituwid, iret )
C************************************************************************
C* GQTURB								*
C*									*
C* This subroutine gets the turbulence symbol attributes.		*
C*									*
C* GQTURB ( SZTURB, ITUWID, IRET )					*
C*									*
C* Output parameters:							*
C*	SZTURB		REAL		Turbulence symbol size		*
C*	ITUWID		INTEGER		Turbulence symbol line width	*
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
	    szturb = 0
	    ituwid = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    szturb = stursz
	    ituwid = ltuwid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
