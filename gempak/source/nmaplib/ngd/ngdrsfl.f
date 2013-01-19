	SUBROUTINE NGD_RSFL ( rstfil, filnam, lenf, iret )
C************************************************************************
C* NGD_RSFL								*
C*									*
C* This routine returns the restore file name after searching for a 	*
C* file in the local directory and the tables directories. If no file	*
C* is found in these locations, the original file name is returned.	*
C*									*
C* NGD_RSFL ( RSTFIL, FILNAM, LENF, IRET )				*
C*									*
C* Input parameters:							*
C*	RSTFIL		CHAR*		Suggested restore file name	*
C*									*
C* Output parameters:							*
C*	FILNAM		CHAR*		Actual restore file name	*
C*	LENF		INTEGER		Length of output file name	*
C*	IRET		INTEGER		Return code			*
C*					   -8 = no restore file		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/01	Created					*
C* S. Jacobs/NCEP	 2/02	Added error return for non-existent file*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	rstfil, filnam
C*
	CHARACTER	newfil*256, dirnam*256, tdir*256,
     +			tfile*256, tpath*256, path*256
	LOGICAL		exist
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for the restore file in the local directory first.
C
	CALL FL_PATH ( rstfil, dirnam, filnam, ier )
	path = ' '
	DO WHILE  ( dirnam .ne. ' ' )
	    CALL FL_PATH ( dirnam, tdir, tfile, ier )
	    IF  ( tdir .ne. ' ' )  THEN
		CALL ST_LSTR ( tfile, lenf, ier )
		IF  ( path .eq. ' ' )  THEN
		    tpath = tfile (1:lenf)
		  ELSE
		    tpath = tfile (1:lenf) // '/' // path
		END IF
		path = tpath
	    END IF
	    dirnam = tdir
	END DO
C
	IF  ( path .eq. ' ' )  THEN
	    tpath = 'restore'
	  ELSE
	    tpath = 'restore/' // path
	END IF
	path = tpath
C
	CALL FL_TINQ ( filnam, path, exist, newfil, ier )
C
	IF  ( exist )  THEN
	    filnam = newfil
	  ELSE
C
	    CALL FL_TINQ ( rstfil, ' ', exist, newfil, ier )
C
	    IF  ( exist )  THEN
	    	filnam = newfil
	      ELSE
		filnam = ' '
		iret   = -8
	    END IF
	END IF
C
	CALL ST_LSTR ( filnam, lenf, ier )
C*
	RETURN
	END
