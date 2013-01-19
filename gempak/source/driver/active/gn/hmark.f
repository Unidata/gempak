	SUBROUTINE HMARK ( np, x, y, iret )
C************************************************************************
C* HMARK - GN								*
C*									*
C* This subroutine draws hardware markers on the device.		*
C*									*
C* HMARK ( NP, X, Y, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of markers		*
C*	X (NP)		REAL		X coordinates in device units	*
C*	Y (NP)		REAL		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		12/96	Changed X and Y to reals		*
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
