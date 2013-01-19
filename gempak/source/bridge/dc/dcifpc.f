	SUBROUTINE DC_IFPC  ( loglev, cimnem, civals, ncimn, iret )
C************************************************************************
C* DC_IFPC								*
C*									*
C* This subroutine writes to the decoder log the character	        *
C* interface values that are not "missing", along with the		*
C* corresponding character interface mnemonics.  The verbosity level	*
C* at which to write this output is specified by the user.		*
C*									*
C* DC_IFPC  ( LOGLEV, CIMNEM, CIVALS, NCIMN, IRET )			*
C*									*
C* Input parameters:							*
C*	LOGLEV		INTEGER		Verbosity level			*
C*	CIMNEM (NCIMN)	CHAR*		Character interface mnemonics	*
C*	CIVALS (NCIMN)	CHAR*		Character interface values	*
C*	NCIMN		INTEGER		Number of char interface values	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		 2/98						*
C* D. Kidwell/NCEP	 4/98	Cleaned up prologue and declarations    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cimnem (*), civals (*)
C*
	CHARACTER	logmsg*20
C-----------------------------------------------------------------------
	iret = 0
C
	DO jj = 1, ncimn
	    IF  ( civals ( jj ) (1:1) .ne. ' ' )  THEN
		logmsg = cimnem ( jj )  //  ':  '  //
     +			 civals ( jj )
		CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
	    END IF
	END DO
C*
	RETURN
	END
