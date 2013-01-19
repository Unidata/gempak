	SUBROUTINE HSCOLR  ( icolr, iret )
C************************************************************************
C* HSCOLR - FAX								*
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
C* E. Wehner/EAi	7/96	Adopted to call raster C routine	*
C* S. Maxwell/GSC       6/97    Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
C*	Set the current color.
C
	CALL RSCOLR ( icolr, iret )
C*
	RETURN
	END
