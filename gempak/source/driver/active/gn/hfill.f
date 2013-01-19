	SUBROUTINE HFILL  ( np, ix, iy, iret )
C************************************************************************
C* HFILL - GN								*
C*									*
C* This subroutine fills a polygon on a graphics device.		*
C*									*
C* HFILL  ( NP, IX, IY, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	IX (NP)		INTEGER		X coordinates			*
C*	IY (NP)		INTEGER		Y coordinates			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
