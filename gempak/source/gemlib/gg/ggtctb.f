	SUBROUTINE GG_TCTB ( table, type, itrack, radii, iret )
C************************************************************************
C* GG_TCTB								*
C*									*
C* This subroutine reads the danger area radii values for the TPC and   *
C* CPHC tropical cyclone marine graphics.  The radii values are added   *
C* to the 34kt wind values at each forecast time to determine the       *
C* danger area.                                                         *
C*									*
C* GG_TCTB ( TABLE, TYPE, ITRACK, RADII, IRET )				*
C*									*
C* Input Parameters:							*
C*	TABLE		CHAR*		Table file name			*
C*	TYPE		CHAR*		File name type			*
C*	ITRACK		INTEGER		Number of forecast times        *
C*									*
C* Output Parameters:							*
C*	RADII(ITRACK,2)	REAL		Danger area radii values        *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					 -1 = error opening table       *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/02	From GH_KGTB                            *
C************************************************************************
	CHARACTER*(*) 	table, type	
        REAL       	radii (itrack,*)
C------------------------------------------------------------------------
	iret = 0
        CALL FL_TBOP ( table, type, lun, ier )
C
        IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'FL', ier, table, ierr )
	    iret = -1
            RETURN
        END IF 
C
	DO ii = 1, itrack
            READ ( lun, * ) ihr, radii ( ii, 1 ), radii ( ii, 2 )
        END DO
C
        CALL FL_REWD ( lun, ier )
        CALL FL_CLOS ( lun, ier ) 
C*
	RETURN
	END
