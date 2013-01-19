	SUBROUTINE ST_ILSR ( string, sep, idef, nexp, iarr, num, iret )
C************************************************************************
C* ST_ILSR								*
C*									*
C* This subroutine breaks a string containing a list of integers into	*
C* an array of integers.  The separator for the integers is input as 	*
C* SEP.  If the separator is a blank, multiple blanks will be changed	*
C* to single blanks before the string is processed.  If null strings	*
C* are encountered or fewer than NEXP strings are found in the		*
C* string, the appropriate IARR locations are set to IDEF.		*
C* Range strings (with optional increments) are indicated with a hyphen *
C* (i.e., 3-9 or 3-12-3) and are processed into the IARR array.		*
C*									*
C* ST_ILSR  ( STRING, SEP, IDEF, NEXP, IARR, NUM, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*	SEP		CHAR*1		Separator			*
C*	IDEF		INTEGER		Default value			*
C*	NEXP		INTEGER		Number of expected values	*
C*									*
C* Output parameters:							*
C*	IARR  (NUM)	INTEGER		Array of integer values		*
C*	NUM		INTEGER		Number of values returned	*
C*	IRET		INTEGER		Return code			*
C*				   	  1 = more than NEXP values 	*
C*				   	  0 = normal return		*
C*				  	 -3 = invalid substring		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 2/96   Started w/ST_ILST ;			*
C*				Added range processing			*
C*				Simplified blank compression (ST_RMBL)	*
C************************************************************************
	CHARACTER*(*)	string, sep
	INTEGER		iarr (*)
C*
	CHARACTER	strbuf*160, cchar*1
	CHARACTER	first*12, last*12, inc*12
	LOGICAL		proces
C------------------------------------------------------------------------
	iret = 0
	num  = 0
C
C*	Remove blanks from the input string.
C
	CALL ST_RXBL  ( string, strbuf, isize, iret )
C
C*	Initialize output array.
C
	DO  i = 1, nexp
	    iarr (i) = idef
	END DO
C
C*	Check for zero length input string.
C
	IF  ( isize .ne. 0 )  THEN
C
C*	    Check for separator and find list elements.
C
	    cchar  = sep
	    iend   = 0
	    ibegin = 1
	    DO WHILE  ( ibegin .le. isize )
	        loc = INDEX ( strbuf (ibegin:), cchar )
	        IF  ( loc .eq. 0 )  THEN
		    iend = isize + 1
	        ELSE
		    iend = ibegin + loc - 1
	        END IF
C
C*	        Move list element into output list.
C*	        Check that num <= nexp.
C
	        IF  ( num .ge. nexp )  THEN
		    iret = 1
		    iend = isize + 1
		ELSE
	    	    IF  ( ibegin .ne. iend )  THEN
C
C*	        	Check for range.
C
	        	CALL ST_RANG ( strbuf (ibegin : iend-1), first, last,
     +				inc, itype, iret )
C
C*	        	No range.
C
			IF ( itype .eq. 0 )  THEN
			    CALL ST_NUMB  ( strbuf (ibegin : iend-1), 
     +				ival, ier )
			    IF  ( ier .eq. 0 )  THEN
		    		num = num + 1
				iarr (num) = ival
			    ELSE
				iret = -3
			    END IF
C
C*	        	Range found with possible increment... process.
C
			ELSE
			    proces = .true.
			    CALL ST_NUMB ( first, i1, ier1 )
			    IF ( ier1 .ne. 0 )  proces = .false.
			    CALL ST_NUMB ( last, i2, ier2 )
			    IF ( ier2 .ne. 0 )  proces = .false.
			    IF ( itype .eq. 2 )  THEN
			    	CALL ST_NUMB ( inc, i3, ier3 )
			        IF ( ier3 .ne. 0 )  proces = .false.
			    ELSE
				i3 = 1
				IF ( i1 .gt. i2 )  i3 = -1
			    END IF
			    IF ( proces )  THEN
			    	DO  i = i1, i2, i3
				    IF ( num .ge. nexp )  THEN
				    	iret = 1
					iend = isize + 1
				    ELSE
				    	num = num + 1
				    	iarr ( num ) = i
				    END IF
			    	END DO
			    ELSE
				iret = -3
			    END IF
			END IF
	    	    END IF
		END IF
C*
		ibegin = iend + 1
	    END DO
	END IF
C*
	RETURN
	END
