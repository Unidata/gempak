	SUBROUTINE GH_KGTB ( table, type, numhr, dist, iret )
C************************************************************************
C* GH_KGTB								*
C*									*
C* This subroutine reads the average track error table for the		*
C* Atlantic or Pacific Oceans.						*
C*									*
C* GH_KGTB ( TABLE, TYPE, NUMHR, DIST, IRET )				*
C*									*
C* Input Parameters:							*
C*	TABLE		CHAR*		Table file name			*
C*	TYPE		CHAR*		File name type			*
C*	NUMHR		INTEGER		Number of error distances 	*
C*									*
C* Output Parameters:							*
C*	DIST (NUMHR)	REAL		Error distances (NM)		*
C*	IRET		INTEGER		Return code			*
C*					  3 = not enough points         *
C*					  0 = normal return             *
C*					 -8 = error opening table       *
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01	                   			*
C* D. Kidwell/NCEP	10/01	dist -> real; changed err processing    *
C* D. Kidwell/NCEP	 4/02	Added format, removed rewind            *
C* D. Kidwell/NCEP	 3/03	Checked for not enough points; drop ihr *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	table, type	
        REAL       	dist (*)
C*
	LOGICAL 	done
C------------------------------------------------------------------------
	iret = 0
        CALL FL_TBOP ( table, type, lun, ier )
C
        IF ( ier .ne. 0 ) THEN
	    iret = -8
            RETURN
        END IF 
C
	ii   = 1
	done = .false.
	DO WHILE ( .not. done )
            READ ( lun, 10, IOSTAT=iostat ) dist (ii)
	    IF ( iostat .eq. 0 ) THEN
		ii = ii + 1
		IF ( ii .gt. numhr ) done = .true.
	      ELSE
		iret = 3
		done = .true.
		IF ( ii .gt. 1 ) THEN
		    DO jj = ii, numhr
			dist ( jj ) = RMISSD
		    END DO
		  ELSE
		    iret = -8
		END IF
	    END IF
        END DO
10      FORMAT ( 2X, F8.1 )
C
        CALL FL_CLOS ( lun, ier ) 
C*
	RETURN
	END
