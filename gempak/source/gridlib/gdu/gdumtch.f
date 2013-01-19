	SUBROUTINE GDU_MTCH ( nexp, gdattm, level, ivcord, parm,
     +			      levtyp, nlev, rlevel, icord, nparm,
     +			      prmlst, ntime, timfnd, match, iret )
C************************************************************************
C* GDU_MTCH								*
C*									*
C* This routine checks for a match between the user input grid info	*
C* and the grid header read from a file.				*
C*									*
C* GDU_MTCH ( NEXP, GDATTM, LEVEL, IVCORD, PARM, LEVTYP, NLEV, RLEVEL,  *
C*            ICORD, NPARM, PRMLST, NTIME, TIMFND, MATCH, IRET )	*
C*									*
C* Input parameters:							*
C*	NEXP		INTEGER		Max number of levels		*
C*	GDATTM (2)	CHAR*		Date/times from the grid header	*
C*	LEVEL (2)	INTEGER		Levels from the grid header	*
C*	IVCORD		INTEGER		Vert coord from the grid header	*
C*	PARM		CHAR*		Parameter from the grid header	*
C*	LEVTYP		INTEGER		Level type from the user	*
C*					  0 = no levels input		*
C*					  1 = list of levels		*
C*					  2 = range of levels		*
C*					  3 = all levels		*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NEXP,2)	REAL		Levels or range from the user	*
C*	ICORD		INTEGER		Vert coord from the user	*
C*	NPARM		INTEGER		Number of parameters		*
C*	PRMLST (NPARM)	CHAR*		Parameter list from the user	*
C*	NTIME		INTEGER		Number of times			*
C*	TIMFND (NTIME)	CHAR*		List of times from the user	*
C*									*
C* Output parameters:							*
C*	MATCH		LOGICAL		Match flag			*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log: 								*
C* S. Jacobs/NCEP	9/96						*
C* S. Maxwell/GSC	9/96		Clean up.			*
C************************************************************************
	CHARACTER*(*)	gdattm(2), parm, prmlst(*), timfnd(*)
	INTEGER		level(2)
	REAL		rlevel( NEXP, 2 )
	LOGICAL		match
C*
	CHARACTER	tgdtm*36
C------------------------------------------------------------------------
	iret  = 0
	match = .false.
C
C*	If the user entered ALL, then match is true.
C
	IF  ( timfnd(1) .eq. 'ALL' )  THEN
	    match = .true.
	  ELSE
C
C*	    Concatenate the times.
C
	    CALL ST_LSTR ( gdattm(1), len1, ier )
	    CALL ST_LSTR ( gdattm(2), len2, ier )
	    IF ( len1 .gt. 0 ) THEN           
		IF ( len2 .gt. 0 ) THEN
		    tgdtm = gdattm(1)(:len1) // ':' // gdattm(2)(:len2)
		  ELSE 
		    tgdtm = gdattm(1)
		END IF
	    END IF
C
C*	    Check all the user input times for a match.
C
	    DO  k = 1, ntime
		IF  ( tgdtm .eq. timfnd(k) )  match = .true.
	    END DO
	END IF
C
C*	Check the vertical coordinate.
C
	IF  ( match )  THEN
	    match = .false.
C
C*	    If the user entered ALL, then match is true.
C
	    IF  ( icord .eq. -2 )  THEN
		match = .true.
	      ELSE
C
C*		Otherwise, check for a match.
C
		IF  ( ivcord .eq. icord )  match = .true.
	    END IF
	END IF
C
C*	Check the levels.
C
	IF  ( match )  THEN
	    match = .false.
C
C*	    If the user entered ALL, then match is true.
C
	    IF  ( levtyp .eq. 3 )  THEN
		match = .true.
	      ELSE IF  ( levtyp .eq. 2 )  THEN
C
C*		Check the range of levels.
C
		IF  ( ( ( ( rlevel(1,1) .le. level(1) ) .and.
     +		          ( rlevel(2,1) .ge. level(1) ) ) .or.
     +		        ( ( rlevel(1,1) .ge. level(1) ) .and.
     +		          ( rlevel(2,1) .le. level(1) ) ) ) .and.
     +		      ( level(2) .eq. -1 ) )  match = .true.

	      ELSE
C
C*		Otherwise, check all the individual levels.
C
		DO  k = 1, nlev
		    IF  ( ( level(1) .eq. rlevel(k,1) ) .and.
     +			  ( level(2) .eq. rlevel(k,2) ) )
     +			match = .true.
		END DO
	    END IF
	END IF
C
C*	Check the list of parameters.
C
	IF  ( match )  THEN
	    match = .false.
C
C*	    If the user entered ALL, then match is true.
C
	    IF  ( prmlst(1) .eq. 'ALL' )  THEN
		match = .true.
	      ELSE
C
C*		Otherwise, check list of parameters for a match.
C
		DO  k = 1, nparm
		    IF  ( parm .eq. prmlst(k) )  match = .true.
		END DO
	    END IF
	END IF
C*
	RETURN
	END
