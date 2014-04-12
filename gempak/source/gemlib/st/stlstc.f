	SUBROUTINE ST_LSTC  ( carr, num, sep, string, iret )
C************************************************************************
C* ST_LSTC								*
C*									*
C* This subroutine takes an array of strings and builds a single string	*
C* consisting of each element of the array, separated by SEP.		*
C*									*
C* ST_LSTC  ( CARR, NUM, SEP, STRING, IRET )				*
C*									*
C* Input parameters:							*
C*	CARR  (NUM)	CHAR*		Array of strings 		*
C*	NUM		INTEGER 	Number of strings in the array	*
C*	SEP		CHAR*1		Separator			*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		String 				*
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C*					  2 = Exceeded string size	*
C**									*
C* Log:									*
C* K. Tyle/GSC		 7/96						*
C************************************************************************
	CHARACTER*(*) 	carr (*), sep, string
C*
	CHARACTER	strtmp*160
C------------------------------------------------------------------------
	iret = 0
	string = ' '
C
	IF ( num .gt. 0 ) THEN
	    ilen = LEN ( string )
	    ibeg = 1
	    DO i = 1, num
	    	CALL ST_LDSP ( carr(i), strtmp, lens, ier )
	    	iend = ibeg + lens - 1
C
C*		Check if next element will fit.
C
		IF ( iend .gt. ilen ) THEN
C
C*		    Blank out trailing separator.
C
		    string ( ibeg - 1 : ibeg - 1 ) = ' '
		    iret = 2
		    RETURN
		END IF
	    	string ( ibeg : iend ) = strtmp
	    	IF ( i .lt. num ) THEN
		    string ( iend + 1 : iend + 1 ) = sep(1:1)
		    ibeg = iend + 2
	        END IF
	    END DO
	END IF	
C*
	RETURN
	END

