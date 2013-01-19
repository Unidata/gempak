	SUBROUTINE HRSPLN  ( np, x, y, iret )
C************************************************************************
C* HRSPLN - GN								*
C*									*
C* This subroutine draws special lines on a graphics device.		*
C*									*
C* HRSPLN  ( NP, X, Y, IRET )						*
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
C* D. Keiser/GSC	 4/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	REAL		x (*), y (*)
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
