	SUBROUTINE GQDASH  ( szdsh, iret)
C************************************************************************
C* GQDASH								*
C*									*
C* This subroutine returns the line dashing scale.			*
C*									*
C* GQDASH  ( SZDSH, IRET )						*
C*									*
C* Output parameters:							*
C*	SZDSH		REAL		Line dashing scale		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szdsh = 0
	    iret  = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
            szdsh = sszdsh
	    iret  = NORMAL
	END IF
C*
	RETURN
	END
