      SUBROUTINE HCLEAR  ( iret )
C************************************************************************
C* HCLEAR - FAX								*
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
C* E. Wehner/EAi	7/96	Adopted to call raster routine		*
C* S. Maxwell/GSC       6/97	Documentation changes                  	*
C************************************************************************
C------------------------------------------------------------------------
	CALL RCLEAR ( iret )
C*
	RETURN
	END
