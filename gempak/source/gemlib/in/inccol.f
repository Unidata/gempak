	SUBROUTINE IN_CCOL  ( colors, nexp, icolor, ccolor, iret )
C************************************************************************
C* IN_CCOL								*
C*									*
C* This subroutine converts the input for the COLORS variable into a	*
C* list of colors.  If the number of colors is less than the number     *
C* expected, the last input color will be repeated to fill the buffer.  *
C* If COLORS is blank, the default is color 1.                          *
C*									*
C* A range of colors can be entered as a starting number, ending	*
C* number and increment separated by dashes.  The increment may be	*
C* negative to indicate a decrement.  This is functionally just a	*
C* shorter way of entering a list of color numbers.  For example,	*
C* 9-3--3 or 9-3-3 is the same as specifying 9;6;3.  If the increment	*
C* is omitted, it is assumed to be 1 or -1.				*
C*									*
C* Color coding information is enclosed by parentheses.  If the input	*
C* color string contains color coding, the information is returned	*
C* in the array ccolor and the substrings are replaced by sequential	*
C* negative numbers in the icolor array.				*
C*									*
C* IN_CCOL  ( COLORS, NEXP, ICOLOR, CCOLOR, IRET )			*
C*									*
C* Input parameters:							*
C*	COLORS		CHAR*		COLORS input			*
C*	NEXP		INTEGER		Number of colors		*
C*									*
C* Output parameters:							*
C*	ICOLOR (NEXP)	INTEGER		Color number array		*
C*	CCOLOR (NEXP)	CHAR*		Color coded  string array	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*                                      -15 = badly formed color coding *
C**									*
C* Log:									*
C* S. Maxwell/GSC	 4/97	Copied from IN_COLR  			*
C* D. Kidwell/NCEP       2/98   Fixed prologue, changed colrin length   *
C* T. Lee/GSC		12/98	Fixed FORMAT statement, X -> 1X		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	colors, ccolor(*)
	INTEGER		icolor (*)
C*
	LOGICAL		query
	CHARACTER	cname (LLCLEV)*24, name*16, cnum*12
	CHARACTER	colrin*(LLMXLN), first*4, last*4, inc*4, 
     +			xname*32
C------------------------------------------------------------------------
	iret = 0
	ncol = 0
	jcol = 0
	colrin = colors
C
C*	Check if ? is last character of colors list.
C
	CALL ST_LSTR  ( colrin, ilen, ier)
	IF  ( colrin ( ilen : ilen ) .eq. '?' )  THEN
	    query = .true.
	    colrin ( ilen:ilen ) = ' '
	  ELSE
	    query = .false.
	END IF
C
C*	Check for color coding groups.
C
	CALL ST_LSTR ( colrin, ispos, ier )
	DO WHILE ( ispos .ne. 0 )
	    ispos = INDEX ( colrin, '(' )
	    IF ( ispos .ne. 0 ) THEN
	        iepos = INDEX ( colrin, ')' )
		IF ( ( iepos .ne. 0 ) .and.
     +		     ( iepos .gt. ispos ) ) THEN
		    jcol = jcol + 1
		    ccolor (jcol) = colrin ( ispos : iepos )
C
C*		    Check that string was not truncated.
C
		    IF ( INDEX ( ccolor (jcol), ')' ) .ne. 0 ) THEN
		        CALL ST_INCH ( -jcol, cnum, ier )
		        CALL ST_RPST ( colrin, ccolor(jcol), cnum,
     +			               ipos, colrin, ier )
		      ELSE
			iret = -15
			CALL ST_RPST ( colrin, colrin ( ispos : iepos ),
     +				       '1', ipos, colrin, ier )
			jcol = jcol - 1
		    END IF
		  ELSE
		    iret = -15
		    CALL ST_RPST ( colrin, colrin ( ispos : ilen ),
     +				   '1', ipos, colrin, ier )
		    ispos = 0
		END IF 
	    END IF
	END DO
C
C*	Check for semicolons to separate colors.
C
	CALL ST_CLST  ( colrin, ';', ' ', LLCLEV, cname, kcol, ier )
	IF  ( kcol .eq. 0 )  kcol = 1
C
C*      Loop through each of the colors.
C
        DO  i = 1, kcol 
C
C*	    Check for a range.
C
	    CALL ST_RANG  ( cname (i), first, last, inc, ityp, ier )
C
C*	    First, consider the case that this is not a range.
C
            IF  ( ityp .eq. 0 )  THEN
	        ncol = ncol + 1
C
C*	        Get the color number.
C
		CALL ST_NUMB  ( cname (i), jcolor, ier )
		IF  ( jcolor .eq. IMISSD )  jcolor = 1
		IF  ( ncol .le. nexp )  icolor (ncol) = jcolor
C
C*		Check for color range.
C*		Check for color initialization.
C
              ELSE
C
C*		Decode range.
C
		CALL ST_NUMB  ( first, ifirst, ier )
		CALL ST_NUMB  ( last,  ilast,  ier )
		CALL ST_NUMB  ( inc,   incr,   ier )
	        IF  ( ityp .eq. 1 )  THEN
		    incr  = 1
	          ELSE
		    incr  = ABS  ( incr )
		END IF
	    	IF  ( ifirst .gt. ilast )  incr = - incr
	    	DO  icol = ifirst, ilast, incr
		    ncol = ncol + 1
		    IF  ( ncol .le. nexp )  THEN
		        icolor (ncol) = icol
		    END IF
	        END DO
	    END IF
	END DO
C
C*	Set undefined colors to last color.
C
	DO  i = ncol + 1, nexp
	    icolor (i) = icolor (ncol)
	END DO
C
C*	Write out color information if ? specified by user.
C
	IF  ( query ) THEN
C
C*	    Find out how many colors there are.  Only check MXCLNM
C*	    colors.
C
	    CALL GQNCOL ( nncolr, ier )
	    IF  ( nncolr .ge. MXCLNM )  nncolr = MXCLNM
C
C*	    Write title.
C
	    WRITE (6, 500)
500	    FORMAT ( 2X, 'COLOR', 2X, 'COLOR NAME', 8X, 'RED', 2X,
     +               'GREEN', 2X, 'BLUE', 2X, 'X COLOR NAME' )
C
C*	    Loop through and get names and components of colors.
C
	    DO  i = 1, nncolr
		name = ' '
		CALL GQCOMP  (  i, name, ired, igreen, iblue, xname,
     +				ier )
		IF ( name  .eq. ' ' ) name  = 'redefined '
		IF ( xname .eq. ' ' ) xname = 'redefined '
		WRITE  ( 6, 510 ) i, name, ired, igreen, iblue, xname
510		FORMAT ( 4X, I3, 2X, A16, 1X, I4, 3X, I4, 2X, I4, 2X,
     +                   A32 )
	    END DO
	    WRITE  ( 6, * )
	END IF
C*
	RETURN
	END
