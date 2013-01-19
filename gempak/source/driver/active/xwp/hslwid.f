	SUBROUTINE HSLWID  ( ilwid, iret )
C************************************************************************
C* HSLWID - XWP								*
C*									*
C* This subroutine sets the hardware line width.			*
C*									*
C* HSLWID  ( ILWID, IRET )						*
C*									*
C* Input parameters:							*
C*	ILWID		INTEGER		Line width			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 8/91	XW device driver			*
C* M. desJardins/NMC	12/91	Save irwdth				*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC	 6/97	Documentation changes			*
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Set the line width.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XSLWID ( ilwid, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PSLWID ( ilwid, iret )
	END IF
C*
	RETURN
	END
