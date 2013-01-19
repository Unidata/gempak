	SUBROUTINE HEVENT  ( iret )
C************************************************************************
C* HEVENT - XWP								*
C* 									*
C* This subroutine does event handling for XW.				*
C*									*
C* HEVENT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* J. Whistler/SSAI	12/91	XW device driver			*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
C
C*	    Process the next event.
C
	    CALL XXEVNT ( iret )
	END IF
C*
	RETURN
	END
