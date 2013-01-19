	SUBROUTINE ST_STOI  ( string, nchar, nval, iarray, iret )
C************************************************************************
C* ST_STOI								*
C*									*
C* This subroutine stores a character string in an integer array.	*
C* Four characters are written to each integer.				*
C*									*
C* ST_STOI  ( STRING, NCHAR, NVAL, IARRAY, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING 		CHAR*		String				*
C*	NCHAR		INTEGER		Number of characters to store	*
C*									*
C* Output parameters:							*
C*	NVAL		INTEGER		Number of integers		*
C*	IARRAY (NVAL)	INTEGER		Integer array			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 6/86						*
C* M. desJardins/GSFC	 4/87	Renamed and rewrote			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Wu/GSC             7/00   Initialized iret to 0                   *
C* S. Jacobs/NCEP	 3/09	Changed to use equivalence instead of	*
C*				an internal read			*
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
C*	Check that actual length of string is greater than NCHAR.
C
	iret = 0
	
	lstr   = LEN ( string )
	ichars = MIN ( lstr, nchar )
C
C*	Check that the length of the string is greater than 0.
C
	IF  ( ichars .gt. 0 )  THEN
C
C*	    Compute the number of integers to convert.
C
	    nval = ( ichars - 1 ) / 4 + 1
	    is   = 1
C
C*	    For each integer, move substring into buff in order to
C*	    blank fill.  Put into integer value.
C
	    DO  i = 1, nval
		ie = is + 3
		IF  ( ie .gt. ichars ) ie = ichars
		buff  =  string (is:ie)
		DO  j = 1, 4
		    barray (j) = ICHAR(buff(j:j))
		END DO
		iarray (i) = itemp
		is = is + 4
	    END DO
	END IF
C
	RETURN
	END
