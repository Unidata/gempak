	SUBROUTINE GPOINP  ( device, iret )
C************************************************************************
C* GPOINP								*
C*									*
C* This subroutine gets the input for GPCLOSE.				*
C*									*
C* GPOINP  ( DEVICE, IRET )						*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/94						*
C************************************************************************
	CHARACTER*(*)	device
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, iret )
C
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
