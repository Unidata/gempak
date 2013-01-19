	SUBROUTINE DM_DALL  ( iflno, nkeys, keynam, iloval, ihival,
     +			      iret )
C************************************************************************
C* DM_DALL								*
C*									*
C* This subroutine deletes data for all locations which match the	*
C* given search criteria.  Data for all parts are deleted along		*
C* with the appropriate headers.  This subroutine packs the data	*
C* into large free blocks and is preferred to deleting single 		*
C* parts using DM_DDAT.							*
C*									*
C* DM_DALL  ( IFLNO, NKEYS, KEYNAM, ILOVAL, IHIVAL, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	NKEYS		INTEGER		Number of keys in search	*
C*	KEYNAM (NKEYS)	CHAR*4		Key names			*
C*	ILOVAL (NKEYS)	INTEGER		Minimum values			*
C*	IHIVAL (NKEYS)	INTEGER		Maximum values			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					-13 = no write access		*
C*					-17 = search criteria not met	*
C*					-26 = invalid delete conditions	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C* I. Graffman/RDS	 2/88	Return error for no match		*
C* M. desJardins/GSFC	 6/88	Fixed no match and scratch space	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	keynam (*)
	INTEGER		iloval (*), ihival (*)
C
	CHARACTER*4	type, stype
	INTEGER		isloc (MMKEY), islov (MMKEY), ishiv (MMKEY)
	LOGICAL		match, deldat
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that user has write access to file.
C
	IF  ( .not. wflag (iflno) ) THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Initialize variables.
C
	nums   = 0
	stype  = ' '
	iret   = 0
	deldat = .false.
C
C*	Initialize number of points in scratch space.
C
	msspce = 0
C
C*	Check for valid number of keys.
C
	IF  ( ( nkeys .le. 0 ) .or. ( nkeys .gt. MMKEY ) )  THEN
	    iret = -26
	    RETURN
	END IF
C
C*	Find location of keys.  Check that all keys are either row or
C*	column keys.
C
	DO  i = 1, nkeys
	    CALL DM_FKEY  ( iflno, keynam (i), type, loc, ier )
	    IF  ( ier .ne. 0 )  THEN
		iret = -26
		RETURN
	      ELSE IF ((type .eq. 'ROW') .and. (stype .ne. 'COL')) THEN
		stype = 'ROW'
		nums  = nums + 1
		isloc ( nums ) = loc
		islov ( nums ) = iloval ( i )
		ishiv ( nums ) = ihival ( i )
	      ELSE IF ((type .eq. 'COL') .and. (stype .ne. 'ROW')) THEN
		stype = 'COL'
		nums  = nums + 1
		isloc ( nums ) = loc
		islov ( nums ) = iloval ( i )
		ishiv ( nums ) = ihival ( i )
	      ELSE
		iret = -26
		RETURN
	    END IF
	END DO
C
C*	Locate headers to check for search condition.
C
	IF  ( stype .eq. 'ROW' )  THEN
	    istart = 1
	    iend   = klstrw ( iflno )
	  ELSE
	    istart = krow ( iflno ) + 1
	    iend   = istart + klstcl ( iflno ) - 1
	END IF
C
C*	Check every header.
C
	DO  i = istart, iend
	    match = .false.
	    IF  ( kheadr ( 0, i, iflno ) .ne. IMISSD )  THEN
		match = .true.
		DO  j = 1, nums
		    kval = kheadr ( isloc (j), i, iflno )
		    IF  ( ( kval .lt. islov (j) ) .or.
     +			  ( kval .gt. ishiv (j) ) )  match = .false.
		END DO
C
C*		If row/col matches search conditions, delete all data.
C
		IF  ( match .and. ( stype .eq. 'ROW' ) )  THEN
		    deldat = .true.
		    jj = krow ( iflno )
		    DO  j = 1, klstcl ( iflno )
			jj = jj + 1
			IF  ( kheadr ( 0, jj, iflno ) .ne. IMISSD ) THEN
			    CALL DM_DELT (iflno, i, j, iret )
			END IF
		    END DO
C
C*		    Delete row header.
C
		    CALL DM_DRWH  ( iflno, i, ier )
C*
		  ELSE IF ( match )  THEN
		    deldat = .true.
		    icol  = i - istart + 1
		    DO  j = 1, klstrw ( iflno )
			IF  ( kheadr ( 0, j, iflno ) .ne. IMISSD ) THEN
			    CALL DM_DELT ( iflno, j, icol, iret )
			END IF
		    END DO
C
C*		    Delete column header.
C
		    CALL DM_DCLH  ( iflno, icol, ier )
		END IF
	    END IF
	END DO
	IF (.not. deldat) THEN
	    iret = - 17
	    RETURN
	END IF
C
C*	Add scratch free space to data management free space.
C
	IF  ( msspce .ne. 0 )  CALL DM_WSPC  ( iflno, ier )
C*
	RETURN
	END
