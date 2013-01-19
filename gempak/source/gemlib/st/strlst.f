	SUBROUTINE ST_RLST ( string, sep, rdef, nexp, rarr, num, iret )
C************************************************************************
C* ST_RLST								*
C*									*
C* This subroutine breaks a string containing a list of reals into an	*
C* array of real values.  The separator for the reals is input as 	*
C* SEP.  If the separator is a blank, multiple blanks will be changed	*
C* to single blanks before the string is processed.  If null strings	*
C* are encountered or fewer than NEXP strings are found in the		*
C* string, the appropriate RARR locations are set to RDEF.		*
C*									*
C* ST_RLST  ( STRING, SEP, RDEF, NEXP, RARR, NUM, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*	SEP		CHAR*1		Separator			*
C*	RDEF		REAL		Default value			*
C*	NEXP		INTEGER		Number of expected values 	*
C*									*
C* Output parameters:							*
C*	RARR  (NUM)	REAL		Array of real values		*
C*	NUM		INTEGER		Number of values returned	*
C*	IRET		INTEGER		Return code			*
C*				   	  1 = too many values 		*
C*				   	  0 = normal return		*
C*				  	 -3 = invalid substring		*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/84	Original source for STLIST		*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	 2/85	Modified for ST_RLST			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP         2/96   Increased size of strbuf                *
C************************************************************************
	CHARACTER*(*)	string, sep
	REAL		rarr (*)
C*
	CHARACTER	strbuf*160, cchar*1
C------------------------------------------------------------------------
	iret = 0
	num  = 0
C
C*	Remove blanks from the input string if the separator is not
C*	a blank.
C
	IF  ( sep .ne. ' ' )  THEN
	    CALL ST_RMBL  ( string, strbuf, isize, iret )
	  ELSE
	    CALL ST_RXBL  ( string, strbuf, isize, iret )
	END IF
C
C*	Initialize output array.
C
	DO  i = 1, nexp
	    rarr (i) = rdef
	END DO
C
C*	Check for zero length input string.
C
	IF  ( isize .eq. 0 )  THEN
	    num = 0
C
C*	    Check for separator and find list elements.
C
	  ELSE
	    cchar  = sep
	    iend   = 0
	    ibegin = 1
	    DO WHILE  ( ibegin .le. isize )
	        loc = INDEX  ( strbuf ( ibegin: ), cchar )
	        IF  ( loc .eq. 0 )  THEN
		    iend = isize + 1
	          ELSE
		    iend = ibegin + loc - 1
	        END IF
C
C*	        Move list element into output list.  Check that num <= nexp.
C
	        IF  ( num .ge. nexp )  THEN
		    iret = 1
		  ELSE
		    num = num + 1
	    	    IF  ( ibegin .ne. iend )  THEN
			CALL ST_CRNM  ( strbuf (ibegin:iend -1), val, 
     +					ier )
			IF  ( ier .eq. 0 )  THEN
			    rarr (num) = val
			  ELSE
			    iret = -3
			END IF
	    	    END IF
		END IF
	    ibegin = iend + 1
	    END DO
	END IF
C*
	RETURN
	END
