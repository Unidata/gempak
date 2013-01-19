	SUBROUTINE HSSMTH  ( ismtyp, dens, ietype, tensn, iret )
C************************************************************************
C* HSSMTH - GN								*
C*									*
C* This subroutine sets the line smoothing attributes.			*
C*									*
C* HSSMTH  ( ISMTYP, DENS, IETYPE, TENSN, IRET )			*
C*									*
C* Input parameters:							*
C*	ISMTYP		INTEGER		Smoothing type			*
C*					  0 = none			*
C*					  1 = splines			*
C*	DENS		REAL		Density of intermediate points	*
C*	IETYPE		INTEGER		End point type			*
C*	TENSN		REAL		Line tension			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
