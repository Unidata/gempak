        SUBROUTINE  LS_INIT( iret )
C************************************************************************
C* LS_INIT                                                              *
C*                                                                      *
C* This subroutine initializes the interface values arrays and certain  *
C* report parameters to missing for a new report.  All initialized      *
C* values are in common.                                                *
C*                                                                      *
C* LS_INIT ( IRET )                                                     *
C*                                                                      *
C* Output parameters:                                                   *
C*	IRET           INTEGER           Return code                    *
C*				   	 0 = normal return 	        *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP       1/98   Changed interface, cleaned up code      *
C* D. Kidwell/NCEP       1/98   Added initialization of KWMO and KCOUN  *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'lscmn.cmn'
C------------------------------------------------------------------------
        iret   = 0
C
        iuwind = IMISSD
        iprec  = IMISSD
        ixind  = IMISSD
        lsogr  = 0      
        ipdprc = 0
        hgtlcl = RMISSD
        hgtlcx = RMISSD
        xtmpk  = RMISSD
        xm907g = RMISSD
        xm907s = RMISSD
        xdtfvm = RMISSD
        gums   = RMISSD
        wdgflg = .false.
        ctyflg = .false.
	kwmo   = IMISSD
	kcoun  = ' '
C
        DO i = 1,6
            xswell ( i ) = RMISSD
        END DO
C
        DO i = 1,4
            DO j = 1,4
                xclds ( i,j ) = RMISSD
            END DO
        END DO
C
        Do i = 1, NRIMN
            rivals ( i ) = RMISSD
        END DO
C
        DO i = 1, NCIMN
            civals ( i ) = ' '
        END DO
C*
        RETURN
        END
