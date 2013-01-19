	SUBROUTINE GQRDUC ( filter, iret )
C************************************************************************
C* GQRDUC								*
C*									*
C* This subroutine queries the filter factor for the point reduction	*
C* scheme.								*
C*									*
C* GQRDUC ( FILTER, IRET )						*
C*									*
C* Output parameters:							*
C*	FILTER		REAL		Filter factor for pnt reduction	*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 5/99						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVSET.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    filter = 0.0
	    iret   = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    filter = srfilt
	END IF
C*
	RETURN
	END
