	SUBROUTINE HSTANM  ( iret )
C************************************************************************
C* HSTANM - XWP								*
C* 									*
C* This subroutine starts a new animation sequence.			*
C* 									*
C* HSTANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI		 7/94	Multi-pixmap				*
C* C. Lin/EAI		 8/94	Added call to XXFLSH			*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Initialize the pixmap counter.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XXFLSH ( .true., iret )
	    CALL XSTANM ( iret )
	END IF
C*
	RETURN
	END
