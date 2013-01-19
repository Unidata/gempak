	SUBROUTINE GDESIZ  ( rec, ncol, nrow, iret )
C************************************************************************
C* GDESIZ								*
C*									*
C* This subroutine extracts the grid size in the x- and y-directions.   *
C*									*
C* GDESIZ  ( REC, NCOL, NROW, IRET )					*
C*									*
C* Input parameters:							*
C*	REC		CHAR*		Data record			*
C*									*
C* Output parameters:							*
C*	NCOL		INTEGER		Number of grid columns		*
C*	NROW		INTEGER		Number of grid rows		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -4 = invalid size format	*
C*					  -5 = only partial grid	*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/85						*
C* I. Graffman/RDS	 8/86	Restructured searches			*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C************************************************************************
	CHARACTER*(*)	rec
C*
	CHARACTER	string (6)*8
	LOGICAL		firstc
C------------------------------------------------------------------------
	iret = 0
C
C*	Break the record into substrings.
C
	CALL ST_RXBL  ( rec, rec, length, ier )
	CALL ST_CLST  ( rec, ' ', ' ', 6, string, nstr, ier )
	IF  ( nstr .lt. 6 )  THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Check that the first string is "COLUMNS:" and the fourth is
C*	"ROWS:" or vice versa.
C
	IF  ( ( string (1) .eq. 'COLUMNS:' ) .and.
     +	      ( string (4) .eq. 'ROWS: ' ) )  THEN
	    firstc = .true.
	  ELSE IF  ( ( string (1) .eq. 'ROWS:' ) .and.
     +		     ( string (4) .eq. 'COLUMNS:' ) )  THEN
	    firstc = .false.
	  ELSE
	    iret = -4
	    RETURN
	END IF
C
C*	Decode the column and row numbers.
C
	CALL ST_NUMB  ( string (2), num2, ier2 )
	CALL ST_NUMB  ( string (3), num3, ier3 )
	CALL ST_NUMB  ( string (5), num5, ier5 )
	CALL ST_NUMB  ( string (6), num6, ier6 )
	IF  ( ( ier2 .ne. 0 ) .or. ( ier3 .ne. 0 ) .or. 
     +	      ( ier5 .ne. 0 ) .or. ( ier6 .ne. 0 ) )  THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Check that the first row and column number are both 1.
C
	IF  ( ( num2 .ne. 1 ) .or. ( num5 .ne. 1 ) )  THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Return the correct numbers for number of rows and columns.
C
	IF  ( firstc )  THEN
	    ncol = num3
	    nrow = num6
	  ELSE
	    ncol = num6
	    nrow = num3
	END IF
C*
	RETURN
	END
