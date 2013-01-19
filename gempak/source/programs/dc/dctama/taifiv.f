	SUBROUTINE TA_IFIV  ( iret )
C************************************************************************
C* TA_IFIV								*
C*									*
C* This subroutine initializes the interface values arrays for a new	*
C* report.								*
C*									*
C* TA_IFIV  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP 08/06                                		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'tacmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, NRIMN
	    rivals ( ii ) = RMISSD
	END DO
C
	DO ii = 1, NCIMN
	    civals ( ii ) = '        '
	END DO
C
C*      Initialize the multi-level counters to 0.0 .
C
        rivals ( irntqc ) = 0.0
C*
	RETURN
	END
