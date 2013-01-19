	SUBROUTINE HENANM  ( iret )
C************************************************************************
C* HENANM - XW								*
C* 									*
C* This subroutine ends an animation sequence.                          *
C* 									*
C* HENANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI		 8/94		Added call to XXFLSH		*
C* S. Maxwell/GSC        6/97           Documentation changes           *
C************************************************************************
C------------------------------------------------------------------------
C*	End the animation group.
C
	CALL XENANM ( iret )
	CALL XXFLSH ( .true., iret )
C*
	RETURN
	END
