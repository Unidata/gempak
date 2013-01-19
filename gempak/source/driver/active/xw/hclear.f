	SUBROUTINE HCLEAR  ( iret )
C************************************************************************
C* HCLEAR - XW								*
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
C* M. desJardins/NMC	 1/92						*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* C. Lin/EAI	 	 6/97	Add 'S' coordinate			*
C*				Changed XCLEAR calling sequence 	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C*	Clear the pixmap.
C
	CALL XCLEAR ( idwdth, idhght, iwdth, ihght, iret )
C
C*	Set the device size.
C
	ibot   = idhght - 1
	iright = idwdth - 1
	iswdth = iwdth
	ishght = ihght 
C*
	RETURN
	END
