	SUBROUTINE HMARK  ( np, x, y, iret )
C************************************************************************
C* HMARK - VG								*
C*									*
C* This subroutine draws markers to the device.				*
C*									*
C* HMARK  ( NP, X, Y, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	X     (NP)	REAL		X coordinates			*
C*	Y     (NP)	REAL		Y coordinates			*
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
	CALL VMARK ( np, x, y, iret )
C*
	RETURN
	END
