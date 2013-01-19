	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - VG								*
C*									*
C* This subroutine opens a plot file for the VG device.			*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET	INTEGER		Return code				*
C**									*
C* Log:									*
C* J. Wu/GSC	02/01   	Created based on hopen.f for PS device	*
C************************************************************************
C------------------------------------------------------------------------
	CALL VOPEN ( iret )
C*
	RETURN
	END
