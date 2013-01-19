	SUBROUTINE HCLEAR  ( iret )
C************************************************************************
C* HCLEAR - NC								*
C* 									*
C* This subroutine clears the current device.  On a direct access 	*
C* device, HCLEAR erases the screen.  On a continuous paper plotter, 	*
C* HCLEAR will advance  to the next page.  On a single page plotter, 	*
C* HCLEAR will unload the paper so another sheet can be loaded. 	*
C* 									*
C* HCLEAR  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/90						*
C* A. Chang/EAI	 	 4/94						*
C* A. Chang/EAI	 	 5/94		Eliminate MSCOLR		*
C* S. Jacobs/NMC	 6/94		General clean up		*
C* S. Maxwell/GSC	 6/97		Documentation changes		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL MCLEAR ( iret )
C*
	RETURN
	END
