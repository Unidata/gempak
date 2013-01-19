	SUBROUTINE HSLWID  ( ilwid, iret )
C************************************************************************
C* HSLWID - XW								*
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
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
C*	Set the line width.
C
	CALL XSLWID ( ilwid, iret )
C*
	RETURN
	END
