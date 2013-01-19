	SUBROUTINE HFILL  ( np, ix, iy, iret )
C************************************************************************
C* HFILL - RBK								*
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
C* A. Hardy/GSC		9/98		Modified from utf's HFILL       *
C************************************************************************
	INCLUDE         'DEVACT.CMN'
C*
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	CALL AFILL ( np, ix, iy, iret )
C*
	RETURN
	END
