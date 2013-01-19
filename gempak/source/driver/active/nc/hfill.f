	SUBROUTINE HFILL  ( np, ix, iy, iret )
C************************************************************************
C* HFILL - NC								*
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
C* M. desJardins/NMC	01/92						*
C* K. Brill/NMC		09/92	Scale plot to subset area		*
C* A. Chang/EAI          4/94                                           *
C* S. Jacobs/NMC	 6/94	General clean up			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL MFILL ( np, ix, iy, iret )
C*
	RETURN
	END
