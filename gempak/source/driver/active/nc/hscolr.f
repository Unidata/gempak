	SUBROUTINE HSCOLR  ( icolr, iret )
C************************************************************************
C* HSCOLR - NC								*
C* 									*
C* This subroutine sets the color on a graphics device.			*
C* 									*
C* HSCOLR  ( ICOLR, IRET )						*
C* 									*
C* Input parameters:							*
C*	ICOLR		INTEGER		Color number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI          4/94                                           *
C* S. Jacobs/NMC	 6/94		General clean up		*
C* S. Maxwell/GSC        6/97    	Documentation changes           *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL MSCOLR ( icolr, iret )
C*
	RETURN
	END
