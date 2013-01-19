	SUBROUTINE HWIND  ( iwnd, np, x, y, spd, dir, iret )
C************************************************************************
C* HWIND - GN								*
C*									*
C* This subroutine draws winds to the device.				*
C*									*
C* HWIND  ( IWND, NP, X, Y, SPD, DIR, IRET )				*
C*									*
C* Input parameters:							*
C*	IWND		INTEGER		Symbol category			*
C*					  1 = Barbs			*
C*					  2 = Arrows			*
C*	NP		INTEGER		Number of points		*
C*	X   (NP)	REAL		X coordinates			*
C*	Y   (NP)	REAL		Y coordinates			*
C*	SPD (NP)	REAL		Wind speed			*
C*	DIR (NP)	REAL		Wind direction			*
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
	REAL		x (*), y (*), spd (*), dir (*)
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
