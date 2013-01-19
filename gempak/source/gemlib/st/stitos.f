	SUBROUTINE ST_ITOS  ( iarray, nval, nchar, string, iret )
C************************************************************************
C* ST_ITOS								*
C*									*
C* This subroutine decodes an array of integers which contain		*
C* four characters each into a single character string.			*
C*									*
C* ST_ITOS  ( IARRAY, NVAL, NCHAR, STRING, IRET )			*
C*									*
C* Input parameters:							*
C*	IARRAY (NVAL)	INTEGER		Integer array			*
C*	NVAL		INTEGER		Number of integers		*
C*									*
C* Output parameters:							*
C*	NCHAR		INTEGER		Number of characters		*
C*	STRING 		CHAR*		Character string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Jacobs/NCEP	 3/09	Changed to use equivalence instead of	*
C*				an internal write			*
C* S. Jacobs/NCEP	12/11	Removed unused variable - iostat	*
C************************************************************************
	CHARACTER*(*)	string
	INTEGER		iarray (*)
C*
	CHARACTER	buff*4
        BYTE		barray(4)
	INTEGER		itemp
C*
	EQUIVALENCE	( itemp, barray )
C------------------------------------------------------------------------
C*	Initialize string.
C
	string = ' '
	iret   = 0
C
C*	Compute the number of output characters.  Check the actual
C*	length of the string.
C
	lstr  = LEN  ( string )
	nchar = nval * 4
	nchar = MIN  ( nchar, lstr )
C
C*	Compute number of integers to read.
C
	ival = ( nchar - 1 ) / 4 + 1
C
C*	Change each integer into a string.
C
	is  = 1
	DO  i = 1, ival
	    ie = is + 3
	    IF  ( ie .gt. nchar )  ie = nchar
	    itemp = iarray (i)
	    DO  j = 1, 4
		buff(j:j) = CHAR(barray(j))
	    END DO
	    n = ie - is + 1
	    string ( is:ie ) = buff ( 1:n )
	    is = is + 4
	END DO
C*
	RETURN
	END
