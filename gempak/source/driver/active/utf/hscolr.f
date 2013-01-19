	SUBROUTINE HSCOLR  ( icolr, iret )
C************************************************************************
C* HSCOLR - UTF								*
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
C* S. Jacobs/GSC	 8/97	Copied for the UTF driver		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
	CALL USCOLR ( icolr, iret )
C*
	RETURN
	END
