	SUBROUTINE HHASH  ( np, x, y, dir, iret )
C************************************************************************
C* HHASH - GN								*
C*									*
C* This subroutine draws hash marks on a graphics device.		*
C*									*
C* HHASH  ( NP, X, Y, DIR, IRET )					* 
C*									* 
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	X (NP)		REAL		X coordinates			*
C*	Y (NP)		REAL		Y coordinates			*
C*	DIR (NP)	REAL		Hash mark direction		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 4/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	REAL		x (*), y (*), dir (*)
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
