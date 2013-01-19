	SUBROUTINE AF_IFIV  ( iret )
C************************************************************************
C* AF_IFIV								*
C*									*
C* This subroutine initializes the interface values arrays for a new	*
C* report.								*
C*									*
C* AF_IFIV  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		08/97						*
C* J. Ator/NCEP		09/99	Initialize ntrb = nicg = ncld = npwx = 0*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, NRIMN
	    rivals ( ii ) = RMISSD
	END DO
	rivals ( irntrb ) = 0
	rivals ( irnicg ) = 0
	rivals ( irncld ) = 0
	rivals ( irnpwx ) = 0
C
	DO ii = 1, NCIMN
	    civals ( ii ) = '        '
	END DO
C*
	RETURN
	END
