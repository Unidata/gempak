	SUBROUTINE HSTANM  ( iret )
C************************************************************************
C* HSTANM - XW/GF							*
C* 									*
C* This subroutine starts a new animation sequence.			*
C* 									*
C* HSTANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Piper/SAIC	02/08	New for GF				*
C************************************************************************
C
C*  Flush the requested device.
C
	CALL GFFLSH ( .true., iret )

C       
C*  Initialize the pixmap counter.
C
	CALL XSTANM ( iret )
C
	RETURN
	END
