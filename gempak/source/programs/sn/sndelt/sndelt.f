	PROGRAM SNDELT
C************************************************************************
C* SNDELT								*
C*									*
C* This program deletes data from a sounding data file.			*
C*									*
C* Log:									*
C* M. desJardins/GSFC	10/88	From SFDELT				*
C* K. Brill/NMC         10/90   Declare TTT				*
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* L. Williams/EAI	 6/94	Removed call to SNDUPD			*
C* K. Tyle/GSC	 	 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized snfcur		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), dattim*(LLMXLN), area*(LLMXLN)
C*
	LOGICAL		respnd, done, proces, allflg
	CHARACTER	timdst (LLMXTM)*20
	CHARACTER	times  (LLMXTM)*20, ttt*20
	CHARACTER	stn*8, snfcur*72, filnam*72
C-----------------------------------------------------------------------
	snfcur = ' '
C
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SNDELT', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SNDELT', ier )
C
C*	Main loop.
C
	done   = .false.
	isnfln = 0
	DO WHILE  ( .not. done )
C
C*	    Get user input and exit if there is an error.
C
	    CALL SNDINP  ( snfile, dattim, area, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SNDELT', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Open the sounding data file.
C
	    CALL FL_MFIL ( snfile, ' ', filnam, iret )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	    CALL SNDFIL  ( filnam, snfcur, isnfln, ntdset, timdst, ier )
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
		    CALL LC_SARE  ( area, isnfln, stn, ier )
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
		CALL SNDOPT  ( times (itime), filnam, area, 
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
		    CALL SN_DTIM  ( isnfln, times (itime), ier )
		    IF  ( ier .ne. 0 )  THEN
			CALL ER_WMSG ('SN', ier, times (itime), ierr)
			CALL ER_WMSG ('SNDELT', -3, times (itime), ier)
		    END IF
		  ELSE
C
C*		    Set the time.  Then loop through deleting data from
C*		    stations.
C
		    CALL SN_STIM  ( isnfln, times (itime), ier )
		    ier = 0
		    DO WHILE  ( ier .eq. 0 )
			CALL SN_SNXT  ( isnfln, stn, istnm, slat,
     +					slon, selv, ier )
			IF  ( ier .eq. 0 )  CALL SN_DDAT ( isnfln, i )
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
	CALL SN_CLOS  ( isnfln, ier )
C
C*	Exit.
C
  	CALL IP_EXIT  ( iret )
	END
