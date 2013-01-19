      SUBROUTINE HCLEAR  ( iret )
C************************************************************************
C* HCLEAR - PS								*
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
C* M. desJardins/GSFC	12/90						*
C* M. desJardins/NMC	 4/91	Check that something has been plotted	*
C* A. Chang/EAI		 1/94	Modified to call C routine		*
C* S. Maxwell/GSC	 6/97	Documentation changes			*
C************************************************************************
C------------------------------------------------------------------------
	CALL PCLEAR ( iret )
C*
	RETURN
	END
