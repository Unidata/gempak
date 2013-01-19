	SUBROUTINE SHN_IFIV  ( iret )
C************************************************************************
C* SHN_IFIV								*
C*									*
C* This subroutine initializes the interface arrays for a new report.	*
C*									*
C* SHN_IFIV  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	nimn = 0
C
	DO ii = 1, MXIMN
	    cimnem ( ii ) = ' '
	    civals ( ii ) = ' '
	    rimnem ( ii ) = ' '
	    rivals ( ii ) = RMISSD
	END DO
C*
	RETURN
	END
