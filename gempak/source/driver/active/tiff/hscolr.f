	SUBROUTINE HSCOLR  ( icolr, iret )
C************************************************************************
C* HSCOLR - TIFF							*
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
C* S. Jacobs/NCEP	12/98						*
C************************************************************************
C------------------------------------------------------------------------
C*	Set the current color.
C
	CALL TSCOLR ( icolr, iret )
C*
	RETURN
	END
