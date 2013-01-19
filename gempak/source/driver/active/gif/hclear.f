	SUBROUTINE HCLEAR  ( iret )
C************************************************************************
C* HCLEAR - GIF								*
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
C* D. Austin		 5/96	Modified for GIF driver			*
C* T. Lee/GSC		 7/00	Renamed gdr_clear to wclear		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'driver.cmn'
C------------------------------------------------------------------------
	iret = NORMAL
C
	IF  ( ( .not. opnfil ) .or. ( .not. gfplot ) )  RETURN
C
C*	Clear the current device.
C
	CALL WCLEAR ( iret )
C*
	RETURN
	END
