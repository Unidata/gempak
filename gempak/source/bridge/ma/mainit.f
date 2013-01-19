        SUBROUTINE MA_INIT ( iret )
C************************************************************************
C* MA_INIT                                                              *
C*                                                                      *
C* This subroutine initializes the interface values arrays and certain  *
C* report parameters to missing for a new report.  All initialized      *
C* values are in common.                                                *
C*                                                                      *
C* MA_INIT ( IRET )	       	                                        *
C*                                                                      *
C* Output parameters:                                                   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP       6/96                                           *
C* K. Tyle/GSC		 4/97	Cleaned up				*
C* D. Kidwell/NCEP	 4/97	Added argument rarray                   *
C* D. Kidwell/NCEP	 4/97	Added initialization for ixind          *
C* R. Hollern/NCEP      10/97   Added initialization for wsped          *
C* D. Kidwell/NCEP	10/97	Changed interface, cleaned up		*
C* F. J. Yen/NCEP	 4/01	Corrected spelling in log		*
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'macmn.cmn'
C-----------------------------------------------------------------------
        iret   = 0
C
        iuwind = IMISSD
        iprec  = IMISSD
	ixind  = IMISSD
        hgtlcl = RMISSD
        hgtlcx = RMISSD
        iflgco = 0
        xm907  = RMISSD
        xdtfvm = RMISSD
        gums   = RMISSD
        wdgflg = .false.
C
        DO i = 1, 6
	    xswell ( i ) = RMISSD
        END DO
C
        DO j = 1, 4
	    DO i = 1, 4
		xclds ( i, j ) = RMISSD
	    END DO
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
