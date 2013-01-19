      SUBROUTINE HCLEAR  ( iret )
C************************************************************************
C* HCLEAR - RBK								*
C* 									*
C* This subroutine clears the current device.  On a direct access 	*
C* device, HCLEAR erases the screen.  On a continuous paper plotter, 	*
C* HCLEAR will advance  to the next page.  On a single page plotter, 	*
C* HCLEAR will unload the paper or place the plotter into pause mode so *
C* another sheet can be loaded.						*
C* 									*
C* HCLEAR  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		9/98		Modified from utf's HCLEAR      *
C************************************************************************
C------------------------------------------------------------------------
	CALL ACLEAR ( iret )
C*
	RETURN
	END
