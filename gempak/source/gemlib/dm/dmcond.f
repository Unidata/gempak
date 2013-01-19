	SUBROUTINE DM_COND  ( iflno, isrch, irow, icol, sflag, iret )
C************************************************************************
C* DM_COND								*
C*									*
C* This subroutine checks whether the search conditions for a single	*
C* search are met.							*
C*									*
C* DM_COND  ( IFLNO, ISRCH, IROW, ICOL, SFLAG, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISRCH		INTEGER		Search number			*
C*	IROW		INTEGER		Row to check			*
C*	ICOL		INTEGER		Column to check			*
C*									*
C* Output parameters:							*
C*	SFLAG		LOGICAL		Result of search		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	LOGICAL		sflag
C-----------------------------------------------------------------------
	iret = 0
C
C*	Compute which header corresponds to this column.
C
	jcol = krow ( iflno ) + icol
C
C*	Check that the row and column headers are defined.
C
	sflag = .true.
	IF  ( kheadr ( 0, irow, iflno ) .eq. IMISSD )  THEN
	    sflag = .false.
	    RETURN
	END IF
C
	IF  ( ( icol .ne. 0 ) .and. ( kheadr ( 0, jcol, iflno ) .eq.
     +				IMISSD ) )  THEN
	    sflag = .false.
	    RETURN
	END IF
C
C*	Check that this is a valid search.
C
	IF  ( isrch .eq. 0 )  THEN
	    IF  ( .not. srcflg ( iflno ) )  RETURN
	  ELSE IF  ( isrch .gt. nsrch ( iflno ) ) THEN
	    RETURN
	END IF
C
C*	Check row conditions.
C
	DO  i = 1, ksnrow  ( isrch, iflno )
	    loc = kslrow ( i, isrch, iflno )
	    IF (( kheadr (loc,irow,iflno) .lt. ksrlov (i,isrch,iflno))
     +					.or.
     +		( kheadr (loc,irow,iflno) .gt. ksrhiv (i,isrch,iflno)))
     +					THEN
		sflag = .false.
		RETURN
	    END IF
	END DO
C
C*	Only check column conditions if column is not 0.
C
	IF  ( icol .eq. 0 )  RETURN
C
C*	Check column conditions.
C
	DO  i = 1, ksncol ( isrch, iflno )
	    loc = kslcol ( i, isrch, iflno )
	    IF (( kheadr (loc,jcol,iflno) .lt. ksclov (i,isrch,iflno))
     +					.or.
     +		( kheadr (loc,jcol,iflno) .gt. kschiv (i,isrch,iflno)))
     +					THEN
		sflag = .false.
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
