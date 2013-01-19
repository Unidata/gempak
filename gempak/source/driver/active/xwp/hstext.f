	SUBROUTINE HSTEXT  ( itxfn, itxhw, sztext, itxwid,
     +			     ibrdr, irrotn, ijust, txtsiz, iret )
C************************************************************************
C* HSTEXT - XWP								*
C* 									*
C* This subroutine sets the text attributes for hardware text. Not all	*
C* information is necessary for every device driver. For some devices,	*
C* the font number and size information must be saved in a common area	*
C* to be retrieved when the text is drawn. This subroutine will be	*
C* called only when hardware text is available.				*
C* 									*
C* HSTEXT ( ITXFN, ITXHW, SZTEXT, ITXWID, IBRDR, IRROTN, IJUST, TXTSIZ,	*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C* 	ITXFN		INTEGER		Text font			*
C* 	ITXHW		INTEGER		Text sw/hw flag			*
C* 	SZTEXT		REAL		Text size			*
C* 	ITXWID		INTEGER		Text width			*
C* 	IBRDR		INTEGER		Text border/blank fill flag	*
C* 	IRROTN		INTEGER		Text north-relative rot flag	*
C* 	IJUST		INTEGER		Text justification		*
C*									*
C* Output parameters:							*
C*	TXTSIZ		REAL		Actual text size set		8
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 8/91	XW device driver			*
C* J. Whistler/SSAI	10/91	Changed method of choosing fonts	*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 9/97	Changed calling sequence		*
C* S. Jacobs/NCEP	 7/98	Added actual text size			*
C* S. Jacobs/NCEP	 7/98	Removed txszx and txszy			*
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Set the hardware text font and size.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XSTEXT ( itxfn, sztext, ijust, txtsiz, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PSTEXT ( itxfn, sztext, ijust, txtsiz, iret )
	END IF
C*
	RETURN
	END
