	SUBROUTINE GSCINT  ( iret )
C************************************************************************
C* GSCINT								*
C*									*
C* This subroutine initializes the colors on the current graphics	*
C* device.  Each device has its own default colors.			*
C*									*
C* GSCINT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	8/85						*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
C------------------------------------------------------------------------
	CALL DSCINT  ( iret )
C*
	RETURN
	END
