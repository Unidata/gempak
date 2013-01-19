	SUBROUTINE UT_EMSG  ( loglev, spname, iersp )
C************************************************************************
C* UT_EMSG								*
C*									*
C* Given a subprogram name and a return code from a previous call to	*
C* that subprogram, this routine writes an error message to the decoder	*
C* log at a specified verbosity level.					*
C*									*
C* UT_EMSG ( LOGLEV, SPNAME, IERSP )					*
C*									*
C* Input parameters:							*
C*	LOGLEV		INTEGER		Verbosity level			*
C*	SPNAME		CHAR*		Subprogram name			*
C*	IERSP		INTEGER		Return code from call to SPNAME	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		06/01						*
C************************************************************************
	CHARACTER*(*)	spname
C*
	CHARACTER*80	logmsg
C*-----------------------------------------------------------------------
	WRITE  ( UNIT = logmsg, FMT = '( 3A, I5 )' )
     +		'Return code from ', spname, ' =', iersp
	CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
C*
	RETURN
	END
