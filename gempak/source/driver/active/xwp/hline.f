	SUBROUTINE HLINE  ( np, ix, iy, iret )
C************************************************************************
C* HLINE - XWP								*
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
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Draw the line.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XLINE ( np, ix, iy, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PLINE ( np, ix, iy, iret )
	END IF
C*
	RETURN
	END
