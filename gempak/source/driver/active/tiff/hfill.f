	SUBROUTINE HFILL  ( np, ix, iy, iret )
C************************************************************************
C* HFILL - TIFF								*
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
C* S. Jacobs/NCEP	12/98						*
C************************************************************************
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	CALL TFILL ( np, ix, iy, iret )
C*
	RETURN
	END
