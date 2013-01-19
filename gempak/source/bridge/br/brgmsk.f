        SUBROUTINE BR_GMSK ( nsky, cmtn, sky, iret )
C************************************************************************
C* BR_GMSK                                                              *
C*                                                                      *
C* This subroutine chooses the three highest priority layers of sky     *
C* cover when more than three layers have been reported.  Table 9-4 in  *
C* FMH-1 is used to determine the priority.				*
C* 								        *
C* BR_GMSK ( NSKY, CMTN, SKY, IRET )           				*
C*								        *
C* Input parameters:							*
C* 	NSKY		INTEGER	     Number of layers (up to 6)         *
C*	CMTN (*)	REAL	     Combined cloud height and coverage *
C*									*
C* Output parameters:						        *
C*	SKY (*)		REAL         Combined cloud height and coverage *
C*      IRET            INTEGER      Return code                        *
C*                                     0 = Normal return                *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* D. Kidwell/NCEP	5/98	                                        *
C* D. Kidwell/NCEP	6/98	Fix to handle '-' flag in sky cover     *
C* D. Kidwell/NCEP	9/02	Renamed from MT_GMSK; removed mtcmn.cmn *
C*				and added args.                         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*  
	REAL		cmtn (*), sky (*)
C*  
	INTEGER 	iwork (6), ipri (8), ikeep (3)
	LOGICAL		found
C*
	DATA		ipri / 6, 3, 4, 2, 2, 3, 3, 2 /
C-----------------------------------------------------------------------
        iret = 0
C
	DO i = 1, nsky
	    asky = ABS ( cmtn ( i ) )
	    iwork ( i ) = INT ( PR_CLCX ( asky ) )
	    IF ( i .lt. 4 ) THEN
		sky ( i )   = RMISSD
		ikeep ( i ) = 0
	    END IF
	END DO
	layer = 0
	ip    = 1
C
C*	Loop to find three highest priority layers reported.
C
	DO WHILE ( layer .lt. 3 )
	    i = 1
	    DO WHILE ( i .le. nsky )
		IF ( iwork ( i ) .eq. ipri ( ip ) ) THEN
		    layer = layer + 1
		    ikeep ( layer ) = i
		    iwork ( i ) = 0
		    i = nsky + 1
		  ELSE
		    i = i + 1
		END IF
	    END DO
	    ip = ip + 1
	    IF ( ip .eq. 9 ) layer = 3
	END DO
C
C*	Fill in with non-priority layers if necessary.
C
	DO layer = 2, 3
	    IF ( ikeep ( layer ) .eq. 0 ) THEN
	        found = .false.
	        i = 1
	        DO WHILE ( .not. found )
		    IF ( iwork ( i ) .ne. 0 ) THEN
		        ikeep ( layer ) = i
		        iwork ( i ) = 0
		        found = .true.
		      ELSE
		        i = i + 1
		        IF ( i .gt. nsky ) found = .true.
		    END IF
	        END DO
	    END IF
	END DO
C
C*	Finally, restore the 3 priority layers to their original order.
C
	DO ilast = 3, 2, -1
	    DO j = 2, ilast
		IF ( ikeep ( j ) .lt. ikeep ( j - 1 ) ) THEN
		    itmp = ikeep ( j )
		    ikeep ( j ) = ikeep ( j - 1 )
		    ikeep ( j - 1 ) = itmp
		END IF
	    END DO
	END DO
C
	DO layer = 1, 3
	    IF ( ikeep ( layer ) .ne. 0 )
     +           sky ( layer ) = cmtn ( ikeep ( layer ) )
	END DO
C*
	RETURN
	END
