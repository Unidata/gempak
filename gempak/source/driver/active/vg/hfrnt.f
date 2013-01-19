	SUBROUTINE HFRNT  ( np, x, y, iret )
C************************************************************************
C* HFRNT - VG								*
C*									*
C* This subroutine draws a front to the device.				*
C*									*
C* HFRNT  ( NP, X, Y, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	X (NP)		REAL		X coordinates in device units	*
C*	Y (NP)		REAL		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	REAL		x (*), y (*)
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL VFRNT ( np, x, y, iret )
C*
	RETURN
	END
