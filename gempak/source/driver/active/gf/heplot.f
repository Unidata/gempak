	SUBROUTINE HEPLOT  ( iret )
C************************************************************************
C* HEPLOT - GF								*
C* 									*
C* This subroutine ends plotting on the device.				*
C* 									*
C* HEPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Piper/SAIC	02/08	New for GF				*	
C************************************************************************
C
C*  Flush the requested device.
C
	CALL GFFLSH ( .false., iret )
C
	RETURN
	END
