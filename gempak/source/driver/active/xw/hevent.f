	SUBROUTINE HEVENT  ( iret )
C************************************************************************
C* HEVENT - XW								*
C* 									*
C* This subroutine does event handling for XW.				*
C*									*
C* HEVENT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* J. Whistler/SSAI	12 /91	XW device driver			*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
C*	Process the next event.
C
	CALL XXEVNT ( iret )
C*
	RETURN
	END
