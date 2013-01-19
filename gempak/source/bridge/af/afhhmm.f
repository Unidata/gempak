	SUBROUTINE AF_HHMM  ( hhmm, iret )
C************************************************************************
C* AF_HHMM								*
C*									*
C* This subroutine decodes and stores the time (i.e. report hour and	*
C* report minutes) from AMDAR, AIREP, PIREP, and RECCO reports.		*
C*									*
C* AF_HHMM  ( HHMM, IRET )						*
C*									*
C* Input parameters:							*
C*	HHMM		CHAR*		Encoded	hour and minutes 	*
C*									*
C* Output parameters:							*
C*	RIVALS (IRHOUR)	REAL		Report hour			*
C*	RIVALS (IRMINU)	REAL		Report minute			*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = error during decoding	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		11/96	Added capability for RECCO reports 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	hhmm
C-----------------------------------------------------------------------
	iret = -1
C
C*	Check the length of the input string.
C
	lhhmm = LEN ( hhmm )
	IF  ( lhhmm .ne. 4 )  THEN
	    logmsg = 'time '  //  hhmm (1:lhhmm) 
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Decode and store the report hour.
C
	CALL ST_INTG  ( hhmm (1:2), ihour, ier )
	IF  ( ( ier .ne. 0 )  .or.
     +		( ( ihour .lt. 0 ) .or. ( ihour .gt. 23 ) )  )  THEN
	    logmsg = 'report hour '  //  hhmm (1:2)
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
	rivals ( irhour ) = FLOAT ( ihour )
C
C*	Decode and store the report minute.
C
	CALL ST_INTG  ( hhmm (3:4), iminu, ier )
	IF  ( ( ier .ne. 0 )  .or.
     +		( ( iminu .lt. 0 ) .or. ( iminu .gt. 59 ) )  )  THEN
	    logmsg = 'report minute '  //  hhmm (3:4)
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
	rivals ( irminu ) = FLOAT ( iminu )
C
	iret = 0
C*
	RETURN
	END
