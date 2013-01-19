	SUBROUTINE HENANM  ( iret )
C************************************************************************
C* HENANM - XWP								*
C* 									*
C* This subroutine ends an animation sequence.                          *
C* 									*
C* HENANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI		 8/94	Added call to XXFLSH			*
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
C*	    End the animation group.
C
	    CALL XENANM ( iret )
	    CALL XXFLSH ( .true., iret )
	END IF
C*
	RETURN
	END
