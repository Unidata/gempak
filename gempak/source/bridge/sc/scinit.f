	SUBROUTINE SC_INIT  ( iret )
C************************************************************************
C* SC_INIT								*
C*									*
C* This subroutine initializes the interface values arrays for a new	*
C* report.								*
C*									*
C* SC_INIT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		12/97		Based on AF_IFIV		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sccmn.cmn'
C*
C-----------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, NRIMN
	    rivals ( ii ) = RMISSD
	END DO
C
	DO ii = 1, NCIMN
	    civals ( ii ) = ' '
	END DO
C*
	RETURN
	END
