	SUBROUTINE IG_IFIV  ( iret )
C************************************************************************
C* IG_IFIV								*
C*									*
C* This subroutine initializes the interface values arrays for a new	*
C* report.								*
C*									*
C* IG_IFIV  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP 06/05                                		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'igcmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, NRIMN
	    rivals ( ii ) = RMISSD
	END DO
C
C*      Initialize the multi-level counters to 0.0 .
C
        rivals ( irnbks ) = 0.0
        rivals ( irnmef ) = 0.0
        rivals ( irnsww ) = 0.0
C
C*
	RETURN
	END
