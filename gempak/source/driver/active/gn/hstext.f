	SUBROUTINE HSTEXT  ( itxfn, itxhw, sztext, itxwid,
     +			     ibrdr, irrotn, ijust, txtsiz, iret )
C************************************************************************
C* HSTEXT - GN								*
C* 									*
C* This subroutine sets the text attributes for hardware text. Not all	*
C* information is necessary for every device driver. For some devices,	*
C* the font number and size information	must be saved in a common area	*
C* to be retrieved when the text is drawn. This subroutine will be	*
C* called only when hardware text is available.				*
C* 									*
C* HSTEXT ( ITXFN, ITXHW, SZTEXT, ITXWID, IBRDR, IRROTN, IJUST, TXTSIZ,	*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C* 	ITXFN		INTEGER		Text font			*
C* 	ITXHW		INTEGER		Text sw/hw flag			*
C* 	SZTEXT		REAL		Text size 			*
C* 	ITXWID		INTEGER		Text width 			*
C* 	IBRDR		INTEGER		Text border/blank fill flag	*
C* 	IRROTN		INTEGER		Text north-relative rot flag	*
C* 	IJUST		INTEGER		Text justification		*
C*									*
C* Output parameters:							*
C* 	TXTSIZ		REAL		Actual text size set		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 9/97	Changed calling sequence		*
C* S. Jacobs/NCEP	 7/98	Added actual text size			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
