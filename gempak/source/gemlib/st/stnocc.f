	SUBROUTINE ST_NOCC  ( string, chocc, nocc, ipoint, iret )
C************************************************************************
C* ST_NOCC   								*
C*									*
C* This subroutine finds the Nth occurrence of a character in a		*
C* string.								*
C*									*
C* ST_NOCC  ( STRING, CHOCC, NOCC, IPOINT, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	CHOCC		CHAR*		Search character		*
C*	NOCC		INTEGER		Occurrence to find		*
C*									*
C* Output parameters:							*
C*	IPOINT		INTEGER		Pointer to Nth occurrence	*
C*	IRET 		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C*					-5 = Nth occurrence not found	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/88						*
C************************************************************************
	CHARACTER*(*)	string, chocc
C*
	LOGICAL		found
C*---------------------------------------------------------------------
	iret   = 0
	ipoint = 0
	found  = .true.
	lens   = LEN ( string )
C
C*	Initialize counter and loop through looking for Nth occurrence.
C
	knt = 1
	DO WHILE  ( found .and. ( knt .le. nocc ) .and. 
     +				( ipoint .lt. lens ) )
C
C*	    Check for next occurrence.
C
	    i  = INDEX  ( string ( ipoint + 1 : ) , chocc ) 
	    IF  ( i .eq. 0 )  THEN
		found = .false.
	      ELSE
		ipoint = ipoint + i
	    END IF
C
C*	    Increment occurrence counter.
C
	    knt = knt + 1
	END DO
C
C*	Check that occurrence was found.
C
	IF  ( ( .not. found ) .or. ( knt .le. nocc ) ) THEN
	    ipoint = 0
	    iret   = -5
	END IF
C*
      RETURN
      END
