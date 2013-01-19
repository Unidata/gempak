	SUBROUTINE GQCVSC  ( dvcvsc, iret )
C************************************************************************
C* GQCVSC								* 
C*									*
C* This subroutine returns the curve scaling factor of a device driver.	*
C*									*
C* GQCVSC  ( DVCVSC, IRET )						*
C*									*
C* Output parameters:							*
C*	DVCVSC		REAL		Device curve scaling factor	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Lee/GSC	 	7/98						*	
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	IF device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    iret    = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVCHR/.
C
	    dvcvsc  = crvscl
	    iret    = NORMAL
	END IF
C*
	RETURN
	END
