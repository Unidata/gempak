	SUBROUTINE HSYMB  ( isym, np, code, x, y, ixoff, iyoff, iret )
C************************************************************************
C* HSYMB - UTF								*
C*									*
C* This subroutine draws symbols to the device.				*
C*									*
C* HSYMB  ( ISYM, NP, CODE, X, Y, IXOFF, IYOFF, IRET )			*
C*									*
C* Input parameters:							*
C*	ISYM		INTEGER		Symbol category			*
C*					  1 = Weather symbols		*
C*					  2 = Cloud type symbols	*
C*					  3 = Icing symbols		*
C*					  4 = Pressure tendency symbols	*
C*					  5 = Past weather symbols	*
C*					  6 = Sky cover symbols		*
C*					  7 = Special symbols		*
C*					  8 = Turbulence symbols	*
C*					  9 = Markers			*
C*	NP		INTEGER		Number of points		*
C*	CODE  (NP)	REAL		Symbol codes			*
C*	X     (NP)	REAL		X coordinates			*
C*	Y     (NP)	REAL		Y coordinates			*
C*	IXOFF (NP)	INTEGER		X offsets in half characters	*
C*	IYOFF (NP)	INTEGER		Y offsets in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 8/97						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	REAL		code (*), x (*), y (*)
	INTEGER		ixoff (*), iyoff (*)
C------------------------------------------------------------------------
	iret = NORMAL
C*
	CALL USYMB ( isym, np, code, x, y, ixoff, iyoff,
     +		     ispanx, ispany, icleft, icrght, icbot, ictop,
     +		     iret )
C*
	RETURN
	END
