	SUBROUTINE DSCINT  ( iret )
C************************************************************************
C* DSCINT								*
C*									*
C* This subroutine initializes the colors on the current graphics	*
C* device.  Each device has its own default colors.			*
C*									*
C* DSCINT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/85						*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* M. desJardins/NMC	12/91	Move check for colcmp to HSCINT		*
C* S. Jacobs/NCEP	11/96	Added reset of current color to 0	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'DEVACT.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	CALL HSCINT  ( iret )
	mcolr = 0
C*
	RETURN
	END
