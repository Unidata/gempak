	SUBROUTINE HFILL  ( np, ix, iy, iret )
C************************************************************************
C* HFILL - PS								*
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
C* J. Whistler/SSAI	10/91						*
C* M. desJardins/NMC	12/91	Buffer output; use symbolic commands	*
C* A. Chang/EAI          2/94   Modified to call C routine              *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE         'DEVACT.CMN'
C*
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	CALL PFILL ( np, ix, iy, iret )
C*
	RETURN
	END
