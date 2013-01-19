	SUBROUTINE HSCOLR  ( icolr, iret )
C************************************************************************
C* HSCOLR - GIF 							*
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
C*J. Nielsen/SUNYA	 3/91	Add colors				*
C* M. desJardins/NMC	12/91	Use symbolic command; check for NNCOLR	*
C* J. Nielsen-G/TAMU	12/97	Updated color range			*
C* T. Lee/GSC		 7/00	Renamed gdr_* routines			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'COLTBL.CMN'
	INCLUDE		'driver.cmn'
C------------------------------------------------------------------------
C*	Make sure plot file is open.
C
	ltz  = -1
	IF  ( .not. opnfil )  THEN
	    CALL HOPEN  ( iret )
	    IF ( iret .ne. NORMAL )  RETURN
	END IF
C
C*	Set color 101 to background color.
C
	IF  ( icolr .eq. 101 )  THEN
C
C	    set current color index to BG color value
C
	    CALL WBCOLR ( ltz, ltz, ltz, iret )
	    resetc = .FALSE.
	    RETURN
	END IF
C
C*	Use only colors NNCOLR colors.
C
	ic = icolr
	IF  ( ( ic .lt. 1 ) .or. ( ic .gt. nncolr ) )  ic = nncolr
C
C*	Write color components to plot file.
C
	CALL WSCOLR ( icred (ic), icgrn (ic), icblue (ic), iret )
C
C*	Now that color has been set, reset flag.
C
	resetc = .FALSE.
C*
	RETURN
	END
