	SUBROUTINE DM_NEXT  ( iflno, irow, icol, iret )
C************************************************************************
C* DM_NEXT								*
C*									*
C* This subroutine returns the location of the next row and column	*
C* meeting the search criteria.						*
C*									*
C* DM_NEXT  ( IFLNO, IROW, ICOL, IRET )					*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IROW		INTEGER		Row number			*
C*	ICOL		INTEGER		Column number			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-17 = search criteria not met	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* m. gamazaychikov/CWS 04/11   Add code for A2DB connectivity          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	LOGICAL		cflag, done, skprow
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Initialize output and flags.
C
	irow  = 0
	icol  = 0
	cflag = .false.
	done  = .false.
	skprow = .false.
C
C*	Set internal pointers to row and column.
C
	jrow = ksrow ( iflno )
	jcol = kscol ( iflno )
C
C*	Check that search is not already done.
C
	IF  ( jrow .gt. klstrw ( iflno ) )  THEN
	    iret = -17
	    RETURN
	END IF
C
C*	Loop through columns and row until search criteria are met
C*	or until end of file is reached.
C
	DO WHILE  ( ( .not. done ) .and. ( .not. cflag ) )
C
C*	    Increment row and column pointers.
C
	    IF  ( ( jrow .eq. 0 ) .and. ( jcol .eq. 0 ) )  THEN
		jrow = 1
	      ELSE IF  ( skprow )  THEN
		jrow = jrow + 1
		jcol = 0
	      ELSE
		jcol = jcol + 1
		IF  ( jcol .gt. klstcl ( iflno ) )  THEN
		    jrow = jrow + 1
		    jcol = 0
		END IF
	    END IF
	    skprow = .false.
C
C*	    Check that end of file has not been reached.
C
	    IF  ( jrow .gt. klstrw ( iflno ) )  THEN
		iret = -17
		done = .true.
	    END IF
C
C*	    Check whether this row and column meet conditions.
C
	    IF  ( .not. done )  THEN
C
C*		Check primary condition first.
C
		CALL DM_COND  ( iflno, 0, jrow, jcol, cflag, iret )
C
C*		Set condition if entire row is being checked.
C
		IF  ( jcol .eq. 0 )  THEN
		    IF  ( cflag )  THEN
			jcol = 1
			CALL DM_COND ( iflno,0,jrow,jcol,cflag,iret)
		      ELSE
			skprow = .true.
		    END IF
		END IF
C
C*		If primary search was successful, check conditional
C*		search criteria.
C
		IF  ( cflag .and. ( nsrch ( iflno ) .gt. 0 ) )  THEN
		  cflag = .false.
		  DO  i = 1, nsrch ( iflno )
C
C*		    Do additive searches only if cflag is false.
C
		    IF ( (.not. cflag) .and. ( kaddsr (i,iflno)) )  THEN
			CALL DM_COND (iflno,i,jrow,jcol,cflag,iret)
C
C*		        Do subtractive search only if cflag is true.
C
		      ELSE IF  ( cflag .and. 
     +					(.not. kaddsr (i,iflno)) ) THEN
			CALL DM_COND (iflno,i,jrow,jcol,cflag,iret)
			cflag = .not. cflag
		    END IF
C
		  END DO
		END IF
	    END IF
	END DO
C
C*	Update current row and column in common.
C
	ksrow ( iflno ) = jrow
	kscol ( iflno ) = jcol
C
C*	If row and column were found, return to user.
C
	IF  ( .not. done )  THEN
	    irow = jrow
	    icol = jcol
	END IF
C*
	RETURN
	END
