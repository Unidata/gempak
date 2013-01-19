	SUBROUTINE HLINE  ( np, ix, iy, iret )
C************************************************************************
C* HLINE - XW								*
C*									*
C* This subroutine draws lines on a graphics device.			*
C*									*
C* HLINE  ( NP, IX, IY, IRET )						*
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
C* J. Whistler/SSAI	 8/91	XW device driver			*
C* J. Whistler/SSAI	10/91	Draw short lines as dots		*
C* M. desJardins/NMC	01/91	GEMPAK 5.1				*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
C*	Draw the line.
C
	CALL XLINE ( np, ix, iy, iret )
C*
	RETURN
	END
