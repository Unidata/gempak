	SUBROUTINE HFILL  ( np, ix, iy, iret )
C************************************************************************
C* HFILL - XW								*
C*									*
C* This subroutine fills in a given polygon on a graphics device.	*
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
C* J. Whistler/SSAI	10/91						*
C* M. desJardins/NMC	01/92	xfpoly-->xfill				*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
C*	Draw the filled polygon.
C
	CALL XFILL ( np, ix, iy, iret )
C*
	RETURN
	END
