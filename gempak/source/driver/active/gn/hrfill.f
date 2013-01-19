	SUBROUTINE HRFILL  ( np, x, y, iret )
C************************************************************************
C* HRFILL - GN								*
C*									*
C* This subroutine draws a filled polygon.				*
C*									*
C* HRFILL  ( NP, X, Y, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	X (NP)		REAL		X coordinates			*
C*	Y (NP)		REAL		Y coordinates			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/97						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	REAL		x (*), y (*)
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
