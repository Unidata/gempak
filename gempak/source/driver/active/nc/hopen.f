	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - NC								*
C*									*
C* This subroutine opens a plot file for the device.  This subroutine	*
C* will be called only by other H subroutines and is used for devices	*
C* which send plotting commands to a plot file.				*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI          1/94           Added func arg filnam		*
C* A. Chang/EAI          4/94           Removed filnam			*
C* S. Jacobs/NMC	 6/94		General clean up		*
C* S. Maxwell/GSC        6/97           Documentation changes           *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL MOPEN ( iret )
C*
	RETURN
	END
