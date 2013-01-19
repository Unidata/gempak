      SUBROUTINE DCLEAR  ( iret )
C************************************************************************
C* DCLEAR								*
C* 									*
C* This subroutine clears the current device.  On a direct access 	*
C* device, DCLEAR erases the screen.  On a continuous paper plotter, 	*
C* DCLEAR will advance  to the next page.  On a single page plotter, 	*
C* DCLEAR will unload the paper so another sheet can be loaded. 	*
C* 									*
C* DCLEAR  ( IRET )							*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER 	Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver to clear screen.
C
	CALL HCLEAR  ( iret )
C*
	RETURN
	END
