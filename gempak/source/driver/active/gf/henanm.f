	SUBROUTINE HENANM  ( iret )
C************************************************************************
C* HENANM - GF								*
C* 									*
C* This subroutine ends an animation sequence.                          *
C* 									*
C* HENANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Piper/SAIC	02/08	New for GF				*
C************************************************************************
C
C*  End the animation group.
C
	CALL XENANM ( iret )
	CALL GFFLSH ( .true., iret )
C
	RETURN
	END
