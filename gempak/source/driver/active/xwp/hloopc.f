	SUBROUTINE HLOOPC  ( icomm, iret )
C************************************************************************
C* HLOOPC - XWP								*
C* 									*
C* This routine will process the animation control commands.		*
C* 									*
C* HLOOPC  ( ICOMM, IRET )						*
C* 									*
C* Input parameters:							*
C*	ICOMM		INTEGER		Animation command		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
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
C*	    Process the animation command.
C
	    CALL XLOOPC ( icomm, iret )
	END IF
C*
	RETURN
	END
