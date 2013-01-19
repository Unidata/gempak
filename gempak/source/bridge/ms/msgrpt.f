	SUBROUTINE MS_GRPT ( bultin, lenbul, ibpnt, report, lenr, iret )
C************************************************************************
C* MS_GRPT								*
C*									*
C* This subroutine gets the data portion of the next MOS report from    *
C* the bulletin, beginning with the first forecast variable name.       *
C*									*
C* MS_GRPT  ( BULTIN, LENBUL, IBPNT, REPORT, LENR, IRET )		*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		Bulletin			*
C*	LENBUL		INTEGER		Bulletin length			*
C*									*
C* Input and output parameters:						*
C*	IBPNT		INTEGER		Pointer in bulletin		*
C*									*
C* Output parameters:							*
C*	REPORT		CHAR*		Report				*
C*	LENR		INTEGER		Length of report		*
C*	IRET		INTEGER		Return code			*
C*					 -2 = no more reports		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00                         			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, report
C-----------------------------------------------------------------------
        iret   = 0 
	report = ' '
	lenr   = 0
	istart = ibpnt
C
C*	Check for end of bulletin.
C
	IF ( istart .gt. lenbul ) THEN
	    report = ' '
	    iret = -2
	    RETURN
	END IF
C
C*	Find the next RS (record separator character).
C
	iposnx = INDEX ( bultin ( istart : lenbul ), CHRS )
C
C*	Determine end of report within the bulletin.
C
	IF  ( iposnx .eq. 0 )  THEN
	    iend = lenbul
	  ELSE
	    iend = istart + iposnx - 2
	END IF
C
C*	Construct report.
C
	lenr = iend - istart + 1
	IF ( lenr .gt. DCMXBF ) lenr = DCMXBF
	report = bultin  ( istart : istart + lenr - 1 )
C
C*	Update pointer in bulletin.
C
	ibpnt = iend + 1
C*
	RETURN
	END
