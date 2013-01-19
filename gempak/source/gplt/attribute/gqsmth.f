	SUBROUTINE GQSMTH ( ismtyp, dens, iret )
C************************************************************************
C* GQSMTH								*
C*									*
C* This subroutine queries the line smoothing attributes.		*
C*									*
C* GQSMTH ( ISMTYP, DENS, IRET )					*
C*									*
C* Output parameters:							*
C*	ISMTYP		INTEGER		Smoothing type			*
C*					  0 = none			*
C*					  1 = splines			*
C*	DENS		REAL		Density of intermediate points	*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 2/98						*
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
	    ismtyp = 0
	    dens   = 0.0
	    iret   = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    ismtyp = lsmtyp
	    dens   = sdens
	END IF
C*
	RETURN
	END
