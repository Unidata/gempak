	SUBROUTINE HLOOPC  ( icomm, iret )
C************************************************************************
C* HLOOPC - XW								*
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
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
C*	Process the animation command.
C
	CALL XLOOPC ( icomm, iret )
C*
	RETURN
	END
