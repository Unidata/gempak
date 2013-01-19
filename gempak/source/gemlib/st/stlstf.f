	SUBROUTINE ST_LSTF  ( rarr, num, sep, ndec, string, iret )
C************************************************************************
C* ST_LSTF								*
C*									*
C* This subroutine takes an array of reals and builds a single string	*
C* consisting of each element of the array, separated by SEP.		*
C*									*
C* ST_LSTF  ( RARR, NUM, SEP, NDEC, STRING, IRET )			*
C*									*
C* Input parameters:							*
C*	RARR  (NUM)	REAL		Array of reals	 		*
C*	NUM		INTEGER 	Number of elements in the array	*
C*	SEP		CHAR*1		Separator			*
C*	NDEC		INTEGER		Number of decimal places	*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		String 				*
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C*					  2 = Exceeded string size	*
C**									*
C* Log:									*
C* K. Tyle/GSC		 7/96		Based on ST_LSTC		*
C* T. Piper/GSC		 3/99		Corrected typo in prolog	*
C************************************************************************
	REAL		rarr(*)
	CHARACTER*(*) 	sep, string
C*
	CHARACTER	strtmp*160
C------------------------------------------------------------------------
	iret = 0
	string = ' '
	ilen = LEN ( string )
C
	IF ( num .gt. 0 ) THEN
	    ibeg = 1
	    DO i = 1, num
	    	CALL ST_RLCH ( rarr(i), ndec, strtmp, ier )
		CALL ST_LSTR ( strtmp, lens, ier )
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

