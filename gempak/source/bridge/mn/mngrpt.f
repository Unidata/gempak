	SUBROUTINE MN_GRPT ( bultin, lenbul, ibpnt, report, lenr, iret )
C************************************************************************
C* MN_GRPT								*
C*									*
C* This subroutine gets the next NGM MOS report from the bulletin.	*
C* Reports begin with the control character RS ( CHRS = 30 ).		*
C*									*
C* MN_GRPT  ( BULTIN, LENBUL, IBPNT, REPORT, LENR, IRET )		*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		Bulletin			*
C*	LENBUL		INTEGER		Bulletin length			*
C*									*
C* Input and Output parameters:						*
C*	IBPNT		INTEGER		Pointer in bulletin		*
C*									*
C* Output parameters:							*
C*	REPORT		CHAR*		Report				*
C*	LENR		INTEGER		Length of report		*
C*	IRET		INTEGER		Return code			*
C*					 -2 = no more reports		*
C**									*
C* Log:									*
C* D. Keiser/GSC	 2/96	Copied from RA_GRPT, modified for MOS	*
C* F. J. Yen/NCEP	11/98	Cleaned up and restructured from AV_GRPT*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, report
C-----------------------------------------------------------------------
        iret = 0 
	lenr = 0
	istart = ibpnt
C
C*	Check that there are not multiple RSs in a row.
C
	DO WHILE  ( ( istart .le. lenbul ) .and. 
     +		    ( bultin ( istart : istart ) .eq. CHRS ) )
	    istart = istart + 1
	END DO
C
C*	Check for end of bulletin.
C
	IF ( istart .gt. lenbul ) THEN
	    report = ' '
	    iret = -2
	    RETURN
	END IF
C
C*	Find the second RS.
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
