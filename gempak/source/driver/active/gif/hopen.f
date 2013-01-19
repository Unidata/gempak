	SUBROUTINE HOPEN  ( iret )
C************************************************************************
C* HOPEN - GIF 								*
C*									*
C* This subroutine opens a plot file for the device.			*
C*									*
C* HOPEN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins	12/90						*
C* J. Nielsen/SUNYA	 3/91	Modify scaling to fit on page		*
C* D. Burks/CSU		 4/91	Set line cap to give rounded edges	*
C* J. Nielsen-G/TAMU	12/96	Modified for gdr			*
C* T. Lee/GSC		 7/00	Renamed gdr_ routines			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'driver.cmn'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*      Initialization.
C 
	IF ( opnfil ) RETURN

        CALL WINIT ( iright + 1, ibot + 1, ier )
C
	IF  ( ier .eq. NORMAL )  THEN
	    opnfil = .true.
	    gfplot = .false.
	END IF
C
C*	Set the color components in the next file.
C
	resetc = .true.
C*
	RETURN
	END
