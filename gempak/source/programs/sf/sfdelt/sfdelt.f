	PROGRAM SFDELT
C************************************************************************
C* SFDELT								*
C*									*
C* This program deletes data from a surface data file.			*
C*									*
C* Log:									*
C* I. Graffman/RDS	12/85						*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	 6/88	Rewrote from SFDELTM			*
C* M. desJardins/GSFC	 1/89	Added declaration for variable TTT	*
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC         4/94   Removed unused variables        	*
C* L. Williams/EAI	 7/94	Removed call to SFDUPD			*
C* K. Tyle/GSC	 	 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* A. Hardy/GSC		 3/99   Added priority parameter to SF_SNXT     *
C* A. Hardy/GSC		 3/99   Removed ispri = 0			*
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized sffcur		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER 	sffile*(LLMXLN), dattim*(LLMXLN), area*(LLMXLN)
C*
	LOGICAL		respnd, done, proces, allflg
	CHARACTER	timdst (LLMXTM)*20, times  (LLMXTM)*20
	CHARACTER	ttt*20, sffcur*72, filnam*72
	CHARACTER	stn*8
C-----------------------------------------------------------------------
	sffcur = ' '
C
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SFDELT', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SFDELT', ier )
C
C*	Main loop.
C
	done   = .false.
	isffln = 0
	DO WHILE (.not. done)
C
C*	    Get user input and exit if there is an error.
C
	    CALL SFDINP  ( sffile, dattim, area, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFDELT', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Open the surface data file.
C
	    CALL FL_MFIL ( sffile, ' ', filnam, iret )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	    CALL SFDFIL  ( filnam, sffcur, isffln, ntdset, timdst, ier )
	    IF  ( ier .ne. 0 )  proces = .false.
C
C*	    Find the times to delete.
C
	    IF  ( proces )  THEN
		CALL TI_FIND  ( dattim, ntdset, timdst, ttt,
     +				ntime,  times,  ier )
		IF  ( ier .ne. 0 )  proces = .false.
	    END IF
C
C*	    Check area for DSET or ALL.  Otherwise, set area.
C
	    IF  ( proces )  THEN
		CALL ST_LCUC  ( area, area, ier )
		IF (( area .eq. 'DSET' ) .or. ( area .eq. 'ALL' )) THEN
		    allflg = .true.
		  ELSE
		    allflg = .false.
		    CALL LC_SARE  ( area, isffln, stn, ier )
		    IF  ( ier .ne. 0 )  proces = .false.
		END IF
	    END IF
C
C*	    Loop through all times.
C
	    itime = 1
	    DO WHILE  ( proces .and. ( itime .le. ntime ) )
C
C*		Give the user a chance to exit.
C
		CALL SFDOPT  ( times (itime), filnam, area, 
     +			       ier )
C
C*		If user wants to exit, do not process any more times.
C
		IF  ( ier .ne. 0 )  THEN
		    proces = .false.
C
C*		    Otherwise, process time.
C
		  ELSE IF  ( allflg )  THEN
C
C*		    Delete all data for this time.
C
		    CALL SF_DTIM  ( isffln, times (itime), ier )
		    IF  ( ier .ne. 0 )  THEN
			CALL ER_WMSG ('SF', ier, times (itime), ierr)
			CALL ER_WMSG ('SFDELT', -3, times (itime), ier)
		    END IF
		  ELSE
C
C*		    Set the time.  Then loop through deleting data from
C*		    stations.
C
		    CALL SF_STIM  ( isffln, times (itime), ier )
		    ier = 0
		    DO WHILE  ( ier .eq. 0 )
			CALL SF_SNXT  ( isffln, stn, istnm, slat,
     +					slon, selv, ispri, ier )
			IF  ( ier .eq. 0 )  CALL SF_DDAT ( isffln, i )
		    END DO
		END IF
C
C*		Go to next time.
C
		itime = itime + 1
	    END DO
C
C*	    Call the dynamic tutor.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Close any file that is open.
C
	CALL SF_CLOS  ( isffln, ier )
C
C*	Exit.
C
  	CALL IP_EXIT  ( iret )
	END
