	SUBROUTINE FL_FPTH  ( dirpth, filnam, fulnam, iret )
C************************************************************************
C* FL_FPTH								*
C*									*
C* This subroutine finds the given file in the list of directories. If	*
C* the file is found, then the full file name, with the directory, is	*
C* returned. The directory list is a colon separated list. The file 	*
C* name is represented by "%N" in each directory. For example:		*
C*	./%N:$HOME/%N:/usr/bin/%N					*
C* or									*
C*	./%N:$NCDESK/config/%N:$GEMTBL/config/%N			*
C*									*
C* FL_FPTH  ( DIRPTH, FILNAM, FULNAM, IRET )				*
C*									*
C* Input parameters:							*
C*	DIRPTH		CHAR*		String of directories to search	*
C*	FILNAM		CHAR*		File name to search for		*
C*									*
C* Output parameters:							*
C*	FULNAM		CHAR*		Full path name			*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/03	Created					*
C************************************************************************
	PARAMETER	( MAXDIR = 100 )
C*
	CHARACTER*(*)	dirpth, filnam, fulnam
C*
	CHARACTER	dirlst (MAXDIR)*256, newfil*256
	LOGICAL		exist, found
C-----------------------------------------------------------------------
	iret   = 0
C
C*	Parse the list of directories into individual items.
C
	CALL ST_CLST ( dirpth, ':', ' ', MAXDIR, dirlst, ndir, ier )
C
C*	Attempt to find the file.
C
	ii    = 1
	found = .false.
	DO WHILE  ( ( .not. found ) .and. ( ii .le. ndir ) )
C
C*	    Replace "%N" with the file name.
C
	    CALL ST_RPST ( dirlst(ii), '%N', filnam, ipos, fulnam, ier )
C
C*	    Check for the existence of the file.
C
	    CALL FL_INQR ( fulnam, exist, newfil, ier )
	    IF  ( exist )  THEN
		found = .true.
	      ELSE
		ii = ii + 1
	    END IF
	END DO
C
C*	If the file was not found, set the output string and return code.
C
	IF  ( .not. found )  THEN
	    fulnam = ' '
	    iret = -16
	END IF
C*
	RETURN
	END
