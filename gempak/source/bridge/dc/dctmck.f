	SUBROUTINE DC_TMCK  ( loglev, irundt, irptdt, imxhro, imxmnn,
     +			      iret )
C************************************************************************
C* DC_TMCK								*
C*									*
C* This subroutine checks the report date-time to make sure that it is	*
C* within a specified number of hours before or specified number of	*
C* minutes after the run date-time.  If the report date-time does not	*
C* fall within the specified range, then a log message is written at a	*
C* specified verbosity level.						*
C*									*
C* DC_TMCK  ( LOGLEV, IRUNDT, IRPTDT, IMXHRO, IMXMNN,			*
C*	      IRET )							*
C*									*
C* Input parameters:							*
C*	LOGLEV		INTEGER		Verbosity level			*
C*	IRUNDT (5)	INTEGER		Run date-time 			*
C*					= (YYYY, MM, DD, HH, MM ) 	*
C*	IRPTDT (5)	INTEGER		Report date-time 		*
C*					= (YYYY, MM, DD, HH, MM ) 	*
C*	IMXHRO		INTEGER		Maximum number of hours by	*
C*					which IRPTDT can precede IRUNDT	*
C*	IMXMNN		INTEGER		Maximum number of minutes by	*
C*					which IRPTDT can follow IRUNDT	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = report date-time is not	*
C*					      within specified range 	*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/98						*
C************************************************************************
	INTEGER		irundt (*), irptdt (*)
C*
	CHARACTER	logmsg*50
C*
C*	Format statement
C*
  111	FORMAT ( 2( A, I4.4, 4I2.2, A ), A, I6 )
C-----------------------------------------------------------------------
	iret = -1
C
	CALL TI_MDIF  ( irundt, irptdt, idif, iermdf )
	IF  ( iermdf .ne. 0 )  THEN
	    CALL DC_WLOG  ( loglev, 'TI', iermdf, ' ', ierwlg )
	ELSE IF  (  ( idif .gt. ( imxhro * 60 ) )  .or.
     +		    ( idif .lt. ( imxmnn * (-1) ) )  )  THEN
	    WRITE  ( logmsg, FMT = 111 )
     +		'rundt ', ( irundt (ii), ii = 1, 5 ), '; ',
     +		'rptdt ', ( irptdt (ii), ii = 1, 5 ), '; ',
     +		'dif ', idif
	    CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
	ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
