	SUBROUTINE HSBRGB  ( icbank, icolr, ired, igreen, iblue, iret )
C************************************************************************
C* HSBRGB - GIF 							*
C*									*
C* This subroutine defines the color corresponding to a color number	*
C* by specifying the values of the red, green, and blue color		*
C* components.  The color components must be in the range 0 - 1.	*
C*									*
C* HSBRGB  ( ICBANK, ICOLR, IRED, IGREEN, IBLUE, IRET )			*
C*									*
C* Input parameters:							*
C*	ICBANK		INTEGER		Color bank number		*
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
C* J. Nielsen-G/TAMU	12/96	Modified after hscrgb for GIF driver	*
C* J. Nielsen-G/TAMU	 5/98	Changed range to 0-255			*
C* T. Lee/GSC		 7/00	Renamed gdr_* to w*; added icbank to	*
C*				wsbrgb; removed gdr_setLUT		*
C* T. Piper/GSC		10/00	Removed check for color 101		*
C* R. Tian/SAIC		05/02	Added fax image type(3)			*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'driver.cmn'
C-----------------------------------------------------------------------
	iret = NORMAL
C
        IF  ( .not. opnfil )  THEN
            CALL HOPEN  ( iret )
            IF  ( iret .ne. NORMAL )  RETURN
        END IF
	IF ( icbank .GT. 3 ) RETURN
C
C*	Apply offset.
C
	IF ( icbank .EQ. 1 ) icolr = icolr + ibgsat - 1
	IF ( icbank .EQ. 2 ) icolr = icolr + ibgrad - 1
	IF ( icbank .EQ. 3 ) icolr = icolr + ibgfax - 1
C
	CALL WSCOLR ( ired, igreen, iblue, iret )
C
C*	Set LUT color.
C
        CALL WSBRGB ( icbank, icolr, ired, igreen, iblue, iret )
C
C*	Check to see if this is the current color.
C
	IF  ( icolr .eq. mcolr )  resetc = .true.
C*
	RETURN
	END
