        SUBROUTINE  MT_INIT ( iret )
C************************************************************************
C* MT_INIT                                                              *
C*                                                                      *
C* This subroutine initializes the interface values arrays and certain  *
C* report parameters to missing for a new report.  All initialized      *
C* values are in common.                                                *
C*                                                                      *
C* MT_INIT ( IRET )                                                     *
C*                                                                      *
C* Output parameters:                                                   *
C*	RIVALS (*)     REAL		 Interface array for real values*
C*	CIVALS (*)     CHAR*		 Interface array for char values*
C*	IRET           INTEGER           Return code                    *
C*				   	   0 = normal return 	        *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* D. Kidwell/NCEP	4/98		Adapted from MT_SET and LS_INIT *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'mtcmn.cmn'
C------------------------------------------------------------------------
        iret   = 0
C
	rmkund = ' '
	contry = ' '
C
	irhour = IMISSD
	DO i = 1, 5
	    irtarr ( i ) = IMISSD
	END DO
C
        DO i = 1, NRIMN
            rivals ( i ) = RMISSD
        END DO
C
        DO i = 1, NCIMN
            civals ( i ) = ' '
        END DO
C*
        RETURN
        END
