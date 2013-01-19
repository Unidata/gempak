	SUBROUTINE GH_WWHI ( nlist, nseg, action, itype, nczmx, part,
     +			     iret )
C************************************************************************
C* GH_WWHI                                                              *
C*									*
C* This subroutine gets the highest priority event type(s) for each     *
C* action code for each part.                                           *
C*                                                                      *
C* GH_WWHI ( NLIST, NSEG, ACTION, ITYPE, NCZMX, PART, IRET )            *
C*                                                                      *
C* Input parameters:                                                    *
C*	NLIST(*)	INTEGER		Number of counties or zones     *
C*	NSEG		INTEGER		Number of segments              *
C*	ACTION(3,*)	CHAR*		Actions - 'NEW', 'CON', 'CAN'   *
C*	ITYPE(3,*)	INTEGER		Event types - 1, 2, 3, 4        *
C*	NCZMX           INTEGER		Max. no. of counties or zones   *
C*									*
C* Input and output parameters:                                         *
C*	PART(NCZMX,*)	CHAR*		County or zone list             *
C*									*
C* Output parameters:                                                   *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 9/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)   action (3,*), part (nczmx,*) 
	INTEGER		nlist (*), itype (3,*)
C-----------------------------------------------------------------------
	iret = 0
	DO iseg = 1, nseg - 1
	  DO ic = 1, nlist ( iseg )
	    DO jj = iseg + 1, nseg
		DO jc = 1, nlist ( jj )
		    IF ( part ( ic, iseg ) .eq. part ( jc, jj ) ) THEN
C
C*			Duplicate counties or zones were found.  Check
C*			for an identical action and choose the one with
C*			the higher event priority.  The lower itype 
C*			value has the higher priority, with itypes 2 and
C*			3 having equal priority.
C*				itype =	1	hurricane warning
C*				itype = 2	hurricane watch
C*				itype = 3	trop. storm warning
C*				itype = 4	trop. storm watch
C
        		IF ( action ( 1, iseg ) .eq. 
     +			     action ( 1, jj ) ) THEN
C
C*			    Check for a hurricane warning.
C
			    IF ( itype ( 1, iseg ) .eq. 1 ) THEN
				IF ( itype ( 1, jj ) .ne. 1 ) THEN
				    part ( jc, jj ) = ' '
				END IF
			      ELSE IF ( itype ( 1, jj ) .eq. 1 ) THEN
				part ( ic, iseg ) = ' '
C
C*			      Check for a tropical storm watch.
C
			      ELSE IF ( itype ( 1, iseg ) .eq. 4 ) THEN
				IF ( itype ( 1, jj ) .ne. 4 ) THEN
				    part ( ic, iseg ) = ' '
				END IF
			      ELSE IF ( itype ( 1, jj ) .eq. 4 ) THEN
				part ( jc, jj ) = ' '
			    END IF
			END IF
		    END IF
		END DO
	    END DO
	  END DO
	END DO
C*
	RETURN
	END
