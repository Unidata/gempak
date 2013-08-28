	SUBROUTINE DM_SRCH ( iflno, type, nkey, keyloc, keyval,
     +			     irwcl, iret )
C************************************************************************
C* DM_SRCH								*
C*									*
C* This subroutine searches a DM file for rows or columns which		*
C* match the given input values.					*
C*									*
C* DM_SRCH  ( IFLNO, TYPE, NKEY, KEYLOC, KEYVAL, IRWCL, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	TYPE		CHAR*		Dimension type : ROW or COL	*
C*	NKEY		INTEGER		Number of keys to search	*
C*	KEYLOC (NKEY)	INTEGER		Key locations			*
C*	KEYVAL (NKEY)	INTEGER		Key values			*
C*									*
C* Output parameters:							*
C*	IRWCL		INTEGER		Search location			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					-17 = search criteria not met	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* J. Whistler/NSSFC	 3/95	Changed the search to be more efficient	*
C* m. gamazaychikov/CWS 04/11   Add code for A2DB connectivity          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	type
	INTEGER		keyloc (*), keyval (*)
	LOGICAL		done
C------------------------------------------------------------------------
	irwcl = 0
C
C*	Check that the file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Find headers to search.
C
	IF  ( type .eq. 'ROW' )  THEN
	    istart = 1
	    istop  = klstrw ( iflno )
	  ELSE IF  ( type .eq. 'COL' )  THEN
	    istart = krow ( iflno ) + 1
	    istop  = istart + klstcl ( iflno ) - 1
	  ELSE
	    iret = -17
	    RETURN
	END IF
C
C*	Loop through all headers looking for match.
C
	done  = .false.
	i     = istart
	DO WHILE (( .not. done ) .and. ( i .le. istop ) )
	    done = .true.
	    j = 1
	    DO WHILE (  ( j .le. nkey ) .and. ( done ) ) 
		IF  ( kheadr ( keyloc (j), i, iflno ) .ne. keyval (j) ) 
     +						done = .false.
		j = j + 1
	    END DO
	    IF  ( done ) irwcl = i
	    i = i + 1
	END DO
C
C*	Correct location when using columns.
C
	IF  ( ( irwcl .ne. 0 ) .and. ( type .eq. 'COL' ) )
     +			irwcl = irwcl - krow ( iflno )
	IF (irwcl .eq. 0) iret = -17
C*
	RETURN
	END
