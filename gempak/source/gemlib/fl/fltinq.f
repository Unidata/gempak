	SUBROUTINE FL_TINQ  ( table, type, exist, fname, iret )
C************************************************************************
C* FL_TINQ								*
C* 									*
C* This subroutine returns the path to table file, if the table exists.	*
C* 									*
C* The TABLE file name is split into the path and filename and the file	*
C* is located by searching in the following order:			*
C* 									*
C*	1. filename (local)						*
C* 	2. path/filename (TABLE as given)				*
C* 	3. $NCDESK/type/filename					*
C* 	4. $NCSITE/type/filename					*
C*	5. $GEMTBL/type/filename					*
C* 									*
C* FL_TINQ  ( TABLE, TYPE, EXIST, FNAME, IRET )				*
C* 									*
C* Input parameters:							*
C*	TABLE		CHAR*		Table file name 		*
C*	TYPE		CHAR*		File name type			*
C*									*
C* Output parameters:							*
C*	EXIST		LOGICAL		Table exists flag		*
C*	FNAME		CHAR*		Full file name for the table	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/01	Copied from FL_TBOP			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	table, type, fname
	LOGICAL		exist
C
	CHARACTER	newfil*132, name*132
	CHARACTER       filnam*132, name3*132, name4*132, name5*132
	CHARACTER	patfil*132, path*132
	LOGICAL		found
C------------------------------------------------------------------------
	iret  = 0
	found = .false.
	fname = ' '
	name  = ' '
C
	IF  ( table .eq. ' ' )  THEN
	    iret  = -1
	    RETURN
	END IF
C
C*	Strip GEMTBL from beginning of the table file name if present.
C
	CALL ST_RMST ( table, '$GEMTBL/', ipos, patfil, ier )
	IF ( ipos .eq. 0 ) THEN
	    CALL ST_RMST ( table, 'GEMTBL:', ipos, patfil, ier )
	END IF
	IF ( ipos .eq. 0 ) patfil = table
C
C*	Extract the filename from the end of the path.
C 
	CALL FL_PATH ( patfil, path, filnam, ier )
	CALL ST_LSTR ( filnam, lens1, ier )
	CALL ST_LSTR ( type, lenst, ier )
C
C*	Check to see if the file is local.
C
	CALL FL_INQR ( filnam, found, newfil, iret )
	IF ( found ) THEN
	    name  = filnam (1:lens1)
	END IF
C
C*	Check to see if file is down a path.
C
	IF ( ( .not. found ) .and. ( path .ne. ' ' ) ) THEN
	    CALL FL_INQR ( patfil, found, newfil, iret )
	    IF ( found ) THEN
		name  = patfil 
	    END IF
	END IF 
C
C*	Check to see if file exists in terms of type in NCDESK. 
C
	IF ( ( .not. found ) .and. type .ne. ' ' ) THEN
	    name3 = '$NCDESK/' // type (1:lenst) // '/' // 
     +		     filnam (1:lens1)
	    CALL FL_INQR  ( name3, found, newfil, iret )
  	    IF  ( found )  THEN
		name  = name3
	    END IF
	END IF
C
C*	Check to see if file exists in terms of type in NCSITE. 
C
	IF ( ( .not. found ) .and. type .ne. ' ' ) THEN
	    name4 = '$NCSITE/' // type (1:lenst) // '/' // 
     +		     filnam (1:lens1)
	    CALL FL_INQR  ( name4, found, newfil, iret )
  	    IF  ( found )  THEN
		name  = name4
	    END IF
	END IF
C
C*	Check to see if file exists in terms of type in GEMTBL.
C
	IF ( .not. found .and. type .ne. ' ' ) THEN
	    name5 = '$GEMTBL/' // type (1:lenst) // '/' // 
     +		     filnam (1:lens1)
	    CALL FL_INQR ( name5, found, newfil, iret )
	    IF ( found ) THEN
		name  = name5
	    END IF
	END IF
C
	fname = name
	exist = found
C*
	RETURN
	END
