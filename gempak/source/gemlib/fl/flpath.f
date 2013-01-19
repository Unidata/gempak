	SUBROUTINE FL_PATH  ( fulnam, dirnam, basnam, iret )
C************************************************************************
C* FL_PATH								*
C*									*
C* This subroutine breaks a full UNIX file path into its directory 	*
C* and file names. This routine does not expand environment variables.	*
C* It also does not check for the existence of the full path name.	*
C*									*
C* FL_PATH  ( FULNAM, DIRNAM, BASNAM, IRET )				*
C*									*
C* Input parameters:							*
C*	FULNAM		CHAR*		Full path name			*
C*									*
C* Output parameters:							*
C*	DIRNAM		CHAR*		Directory			*
C*	BASNAM		CHAR*		Last level of path		*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C* Log:									*
C* K. Tyle/GSC		 5/97  			 			*
C* S. Jacobs/NCEP	11/99	Updated for delivery			*
C************************************************************************
	CHARACTER*(*)	fulnam, dirnam, basnam
C*
	LOGICAL		found
C-----------------------------------------------------------------------
	iret   = 0
	dirnam = ' '
	basnam = fulnam
	found  = .false.
C
C*	Get the length of the input string.
C
	CALL ST_LSTR ( fulnam, lenf, ier )
	IF ( lenf .eq. 0 ) RETURN
C
C*	Start at last character and loop backwards searching for a '/'.
C*	If the full path ends with a '/', continue searching for the
C*	next '/'.
C
	ip = lenf
	DO WHILE  ( ( ip .ge. 1 ) .and. ( .not. found ) )
	    IF  ( fulnam (ip:ip) .eq. '/' )  THEN
		IF  ( ip .ne. lenf )  THEN
		    found  = .true.
		    dirnam = fulnam (:ip)
		    basnam = fulnam (ip+1:)
		END IF
	    END IF
	    ip = ip - 1
	END DO
C
C*	If the base name ends with a '/', remove it.
C
	CALL ST_LSTR ( basnam, lenb, ier )
	IF  ( basnam(lenb:lenb) .eq. '/' )  basnam(lenb:lenb) = ' '
C
C*	If the directory name ends with a '/', remove it, unless
C*	the string contains only a '/'.
C
	CALL ST_LSTR ( dirnam, lend, ier )
	IF  ( lend .gt. 1 )  dirnam(lend:lend) = ' '
C*
	RETURN
	END
