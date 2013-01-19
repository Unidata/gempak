	SUBROUTINE DLOOPC  ( icomm, iret )
C************************************************************************
C* DLOOPC								*
C* 									*
C* This routine will process the animation control commands.		*
C* 									*
C* DLOOPC  ( ICOMM, IRET )						*
C* 									*
C* Input parameters:							*
C*	ICOMM		INTEGER		Animation command		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI		12/93						*
C* S. Jacobs/NMC	 6/94	Added call to HLOOPC			*
C* T. Lee/GSC		 9/97	Included ERROR.PRM			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Send animation command to device.
C
	CALL HLOOPC ( icomm, iret )
C*
	RETURN
	END
