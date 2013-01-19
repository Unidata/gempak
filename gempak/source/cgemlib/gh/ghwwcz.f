	SUBROUTINE GH_WWCZ ( iseg, nlist, nseg, action, itype, nczmx,
     +			     part, iret )
C************************************************************************
C* GH_WWCZ                                                              *
C*									*
C* This subroutine checks for and removes duplicate counties or zones   *
C* when one event type or action takes priority over another.           *
C*                                                                      *
C* GH_WWCZ ( ISEG, NLIST, NSEG, ACTION, ITYPE, NCZMX, PART, IRET )      *
C*                                                                      *
C* Input parameters:                                                    *
C*	ISEG		INTEGER		Current segment number          *
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
C* D. Kidwell/NCEP	11/03						*
C* X. Guo/CWS		03/10	Comment out duplicated codes            *
C*                              GH_WWLD will eliminate the redundant    *
C*                              county/zone codes                       * 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)   action (3,*), part (nczmx,*) 
	INTEGER		nlist (*), itype (3,*)
C-----------------------------------------------------------------------
	iret = 0
	DO ic = 1, nlist ( iseg )
	    DO jj = iseg + 1, nseg
		DO jc = 1, nlist ( jj )
		    IF ( part ( ic, iseg ) .eq. part ( jc, jj ) ) THEN
C
C*			Duplicate counties or zones were found.  Check
C*			for both parts having an action of cancel, or 
C*			both parts having an action of new or continue.
C
        		IF ( ( ( action ( 1,iseg ) .ne. 'CAN' ) .and.
     +			       ( action ( 1,jj   ) .ne. 'CAN' ) ) .or.
     +			     ( ( action ( 1,iseg ) .eq. 'CAN' ) .and.
     +			       ( action ( 1,jj   ) .eq. 'CAN' ) ) ) THEN
C
		     	    IF ( ( ( itype ( 1, iseg ) .eq. 2 ) .and.
     +				   ( itype ( 1, jj   ) .eq. 3 ) ) .or.
     +				 ( ( itype ( 1, iseg ) .eq. 3 ) .and.
     +				   ( itype ( 1, jj   ) .eq. 2 ) ) ) THEN
C
C*				The types hurricane watch and tropical
C*				storm warning have equal priority.  Do
C*				nothing.
C
			      ELSE	
C
C*				The lowest itype value has the highest
C*				priority.
C*				itype =	1	hurricane warning
C*				itype = 2	hurricane watch
C*				itype = 3	trop. storm warning
C*				itype = 4	trop. storm watch
C
			        IF ( itype ( 1, iseg ) .lt. 
     +				     itype ( 1, jj   ) ) THEN
				    part ( jc, jj ) = ' '
			          ELSE IF ( itype ( 1, iseg ) .gt.
     +					    itype ( 1, jj   ) ) THEN
				    part ( ic, iseg ) = ' '
				  ELSE
C
C*				    If the types are the same, choose
C*				    'CON' over 'NEW'.
C				
				    IF ( action ( 1, iseg ) .eq. 'NEW' )
     +					 THEN
					part ( ic, iseg ) = ' '
				      ELSE
					part ( jc, jj ) = ' '
				    END IF
			        END IF
			    END IF
			  ELSE
C
C*			    One part has an action of cancel, and the
C*			    other does not.  Check to see if they are
C*			    the same event type, and if so, choose the
C*			    one that is not 'CAN'.
C
C*                          It is a duplicate and breaks the following
C*                          process. GH_WWLD will eliminate the redundant
C*                          countoes/zones for this case.
C*                          do nothing right now to fix ticket18--TCA 
C*                          Cancel Bug.
C                          
C*			    IF ( itype ( 1, iseg ) .eq. 
C*     +				 itype ( 1, jj   ) ) THEN
C*				IF ( action ( 1,iseg ) .eq. 'CAN' ) THEN
C*				    part ( ic, iseg ) = ' '
C*				  ELSE
C*				    part ( jc, jj ) = ' '
C*				END IF
C*			    END IF
			END IF
		    END IF
		END DO
	    END DO
	END DO
C*
	RETURN
	END
