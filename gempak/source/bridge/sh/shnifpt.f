	SUBROUTINE SHN_IFPT  ( loglev, iret )
C************************************************************************
C* SHN_IFPT								*
C*									*
C* This subroutine prints the interface values arrays to the decoder	*
C* log if the verbosity level is LOGLEV or higher.			*
C*									*
C* SHN_IFPT  ( LOGLEV, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*	LOGLEV		INTEGER		Verbosity level			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C* J. Ator/NCEP		07/06	Print shstyp via DC_IFPC when available.*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	logmsg = '<-----BEGIN INTERFACE OUTPUT----->'
	CALL DC_WLOG ( loglev, 'DC', 2, logmsg, ierwlg )
C
	CALL DC_IFPC ( loglev, cimnem, civals, 2, ierfpc )
C
	CALL DC_IFPR ( loglev, rimnem, rivals, 8, ierfpr )
C
	DO ii = 9, nimn, 3
	    WRITE ( UNIT = logmsg, FMT = '( 4A10 )' )
     +	    rimnem ( ii ), rimnem ( ii + 1 ), rimnem ( ii + 2 ),
     +	    cimnem ( ii + 1 )
	    CALL DC_WLOG ( loglev, 'DC', 2, logmsg, ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( F8.0, F10.4, F8.0, 8X, A)' )
     +	    rivals ( ii ), rivals ( ii + 1 ), rivals ( ii + 2 ),
     +	    civals ( ii + 1 )
	    CALL DC_WLOG ( loglev, 'DC', 2, logmsg, ierwlg )
	END DO
C
	logmsg = '<-----END INTERFACE OUTPUT----->'
	CALL DC_WLOG ( loglev, 'DC', 2, logmsg, ierwlg )
C*
	RETURN
	END
