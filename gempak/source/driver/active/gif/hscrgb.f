	SUBROUTINE HSCRGB  ( icolr, ired, igreen, iblue, iret )
C************************************************************************
C* HSCRGB - GIF 							*
C*									*
C* This subroutine defines the color corresponding to a color number	*
C* by specifying the values of the red, green, and blue color		*
C* components.  The color components must be in the range 0 - 1.	*
C*									*
C* HSCRGB  ( ICOLR, IRED, IGREEN, IBLUE, IRET )				*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*	IRED		INTEGER		Red component			*
C*	IGREEN		INTEGER		Green component			*
C*	IBLUE		INTEGER		Blue component			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* M. desJardins/NMC	12/91	Add reset flag for color components	*
C* J. Nielsen-G/TAMU	12/96	Modified for GIF driver			*
C* J. Nielsen-G/TAMU	 5/98	Changed range to 0-255			*
C* T. Lee/GSC		 7/00	Renamed gdr_* to W* routines		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'COLTBL.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'driver.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
        IF  ( .not. opnfil )  THEN
            CALL HOPEN  ( iret )
            IF  ( iret .ne. NORMAL )  RETURN
        END IF

	IF ( icolr .eq. 101 ) THEN
	    CALL WBCOLR ( ired, igreen, iblue, iret )
	ELSE
	    icred  ( icolr ) = ired
	    icgrn  ( icolr ) = igreen
	    icblue ( icolr ) = iblue
	    CALL  WSCOLR ( ired, igreen, iblue, iret )
	END IF
C
C*	Check to see if this is the current color
C
	IF  ( icolr .eq. mcolr )  resetc = .true.
C*
	RETURN
	END
