	SUBROUTINE DC_IFPR  ( loglev, rimnem, rivals, nrsimn, iret )
C************************************************************************
C* DC_IFPR								*
C*									*
C* This subroutine writes to the decoder log all of the real		*
C* single-level	interface values that are not "missing", along with	*
C* the corresponding real interface mnemonics.  The verbosity level	*
C* at which to write this output is specified by the user.		*
C*									*
C* DC_IFPR  ( LOGLEV, RIMNEM, RIVALS, NRSIMN, IRET )			*
C*									*
C* Input parameters:							*
C*	LOGLEV		 INTEGER     Verbosity level			*
C*	RIMNEM (NRSIMN)	 CHAR*	     Real interface mnemonics	        *
C*	RIVALS (NRSIMN)	 REAL	     Real interface values		*
C*	NRSIMN         	 INTEGER     Number of real single-level values *
C*									*
C* Output parameters:							*
C*	IRET		 INTEGER     Return code 			*
C*				       0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		 2/98						*
C* D. Kidwell/NCEP	 4/98	Cleaned up prologue and declarations    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	rimnem (*)
	REAL		rivals (*)
C*
	INTEGER		idx (5)
	CHARACTER	logmsg*50
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	idxct = 0
C
	DO jj = 1, nrsimn
	    IF  ( .not. ERMISS ( rivals ( jj ) ) )  THEN
		idxct = idxct + 1
		idx ( idxct ) = jj
		IF  ( idxct .eq. 5 )  THEN
		    WRITE ( UNIT = logmsg, FMT = '( 5A10 )' )
     +			( rimnem ( idx ( ii ) ), ii = 1, 5 )
		    CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
		    WRITE ( UNIT = logmsg, FMT = '( 5F10.2 )' )
     +			( rivals ( idx ( ii ) ), ii = 1, 5 )
		    CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
		    idxct = 0
		END IF
	    END IF
	END DO
C
	IF  ( idxct .gt. 0 )  THEN
	    WRITE ( UNIT = logmsg, FMT = '( 5A10 )' )
     +		( rimnem ( idx ( ii ) ), ii = 1, idxct )
	    CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 5F10.2 )' )
     +		( rivals ( idx ( ii ) ), ii = 1, idxct )
	    CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
	END IF
C*
	RETURN
	END
