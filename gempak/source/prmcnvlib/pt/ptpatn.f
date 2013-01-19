	FUNCTION PT_PATN  ( actp )
C************************************************************************
C* PT_PATN								*
C*									*
C* This function converts a PIREP alphanumeric aircraft type (up to four*
C* characters in length) to a real number, ATPN:                        *
C*                                                                      *
C*			ATPN = PT_PATN ( ACTP )                         *
C*                                                                      *
C* If the aircraft type is more than four characters long, the first    *
C* four characters will be used, unless the type contains an embedded   *
C* '/'.  If the type contains an embedded '/', the longer of the fields *
C* on either side of the '/' (to a maximum of four characters) is used. *
C* The range of values which might result is 0 to 2559999.              *
C*									*
C* REAL PT_PATN  ( ACTP )                                                    *
C*									*
C* Input parameters:                                                    *
C*	ACTP		CHAR*		Character aircraft type string  *
C*									*
C* Output parameters:							*
C*	PT_PATN     	REAL 		GEMPAK aircraft type number     *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 1/00	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*	
	CHARACTER*(*)	actp
C*	
	REAL		fnum (4)
C-----------------------------------------------------------------------
	PT_PATN  = RMISSD
C
	DO i = 1, 4
	    fnum ( i ) = 0.
	END DO
	ich = 1
	ist = 1
C
	CALL ST_LSTR ( actp, lens, ier )
	IF ( lens .gt. 4 ) THEN
	    islsh = INDEX ( actp ( :lens ), '/' )
	    IF ( islsh .eq. 0 ) THEN
		lens = 4
	      ELSE
C
C*		Choose the longer of 2 fields separated by '/'.
C
	        len1  = islsh - 1
	        len2  = lens - islsh
		IF ( len1 .eq. len2 ) THEN
		    lens = len1
		  ELSE
		    IF ( len1 .gt. len2 ) THEN
			lens = len1
			ioff = 0
		      ELSE
			lens = len2
			ioff = islsh
		    END IF
		    IF ( lens .gt. 4 ) lens = 4
		    ist  = ioff + 1
		    lens = ioff + lens
		END IF
	    END IF
	END IF
C
	DO ii = ist, lens
	    iv = ICHAR ( actp ( ii:ii ) )
	    IF ( ( iv .ge. 45 ) .and. ( iv .le. 57 ) ) THEN
C
C*	        The character is '-', '.', '/', or '0' through '9'.
C*	        Map it to 1 to 13.
C 
	        fnum ( ich ) = iv - 44
	      ELSE IF ( ( iv .ge. 65 ) .and. ( iv .le. 90 ) ) THEN
C
C*	        The character is 'A' through 'Z'. 
C*	        Map it to 14 to 39.
C 
	        fnum ( ich ) = iv - 51
	      ELSE
C
C*	        The character is not in the valid subset.  
C*	        Map it to 3 ('/').
C 
	        fnum ( ich ) = 3.
	    END IF
	    ich = ich + 1
C
	    IF ( ii .eq. lens ) THEN
	        PT_PATN  = fnum ( 4 ) * 40 * 40 * 40 + 
     +		           fnum ( 3 ) * 40 * 40 +
     +	                   fnum ( 2 ) * 40 + fnum ( 1 )
	    END IF
	END DO
C*
	RETURN
	END
