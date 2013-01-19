	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - UTF								*
C*									*
C* This subroutine opens a plot file for the device.			*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	11/96	Initial coding				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
	CALL UOPEN ( iret )
C*
	RETURN
	END
