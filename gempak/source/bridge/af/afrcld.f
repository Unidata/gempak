	SUBROUTINE AF_RCLD  ( report, lenr, irptr, iret )
C************************************************************************
C* AF_RCLD								*
C*									*
C* This subroutine decodes and stores the cloud data from within a	*
C* RECCO report.							*
C*									*
C* AF_RCLD  ( REPORT, LENR, IRPTR, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		RECCO report 			*
C*	LENR		INTEGER		Length of REPORT 		*
C*									*
C* Input and output parameters:						*
C*	IRPTR		INTEGER		Pointer within REPORT 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRNCLD)	REAL		Number of cloud levels		*
C*	RIVALS (IRCLAM)	REAL		Cloud amount			*
C*	RIVALS (IRCLTP)	REAL		Cloud type			*
C*	RIVALS (IRHCBF)	REAL		Base of cloud in feet  		*
C*	RIVALS (IRHCTF)	REAL		Top of cloud in feet  		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = critical error in report	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		01/97						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 7/99	Changed meters to feet in prologue      *
C* D. Kidwell/NCEP	 8/99	(irhocb,irhoct) -> (irhcbf,irhctf)      *
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C* J. Ator/NCEP		01/02	Use PR_HCDM				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	field*(MXLENF)
C*
	INTEGER		ins ( 3 )
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = -1
	ncld = 0
C
	DO WHILE  ( .true. )
C
C*	    The next group should contain information on the cloud
C*	    layers and cloud amounts.
C
	    CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	    IF  ( ( ier .ne. 0 ) .or. ( lenf .ne. 5 ) .or.
     +		    ( field (1:1) .ne. '1' )  )  THEN
		RETURN
	    END IF
C
C*	    Decode the number of cloud layers.
C
	    CALL ST_INTG  ( field (2:2), ikn, ier )
	    IF  ( ikn .le. 0 )  THEN
		RETURN
	    END IF
C
C*	    For each of the cloud layers, decode the cloud amount.
C
	    DO ii = 1, MIN0 ( ikn, 3 )
		CALL ST_INTG  ( field ( ( ii + 2 ) : ( ii + 2 ) ),
     +				ins ( ii ), ier )
	    END DO
C
C*	    For each of the cloud layers, get and decode the group
C*	    containing information on the cloud type and the altitudes
C*	    of the cloud base and top.
C
	    DO ii = 1, MIN0 ( ikn, 3 )
		CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
		IF  ( ( ier .ne. 0 ) .or. ( lenf .ne. 5 ) ) THEN
		    RETURN
		END IF
C
C*		Decode the cloud type.
C
		CALL ST_INTG  ( field (1:1), ic, ier )
		IF  (  ( ins ( ii ) .ne. IMISSD ) .and.
     +			( ic .ne. IMISSD )  )  THEN
		    IF  ( ncld .ge. MXNLYR )  THEN
			WRITE  ( UNIT = logmsg, FMT = '( I2, A )' )
     +			  MXNLYR, ' cloud layers'
			CALL DC_WLOG  ( 2, 'AF', 3, logmsg, ierwlg )
			RETURN
		    END IF
		    ncld = ncld + 1
		    rivals ( irncld ) = ncld
		    rivals ( irclam ( ncld ) ) = FLOAT ( ins ( ii ) )
		    rivals ( ircltp ( ncld ) ) = FLOAT ( ic )
C
C*		    Decode the altitude of the cloud base.
C
		    rivals ( irhcbf ( ncld ) ) =
     +			PR_HGMF ( PR_HCDM ( field (2:3) ) )
C
C*		    Decode the altitude of the cloud top.
C
		    rivals ( irhctf ( ncld ) ) =
     +			PR_HGMF ( PR_HCDM ( field (4:5) ) )
		END IF
	    END DO
C
C*	    Are there any more cloud layers available?
C
	    IF  ( ikn .le. 3 )  THEN
		iret = 0
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
