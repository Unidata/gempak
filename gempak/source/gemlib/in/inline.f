	SUBROUTINE IN_LINE ( line, values, nexp, icolor, itype, iwidth,
     +			     ilabel, smooth, filter, scflag, iret )
C************************************************************************
C* IN_LINE								*
C*									*
C* This subroutine converts the input for the LINE variable into a	*
C* list of colors, line types, line widths and line label flags.	*
C* If the number of specifications is less than the number expected,	*
C* the input sequence will be repeated to fill the buffer.		*
C* A smoothing level is also specified and applied to all lines.	*
C*									*
C* The LINE input must be of the form:					*
C*									*
C*      col1;col2;.../typ1;typ2.../wid1;wid2.../lab1;lab2.../smth/fltr	*
C*									*
C* In general, lines are turned off by specifying color equal to 0.	*
C* 0 for line type or width will use a default value of 1.  0 for	*
C* line label will suppress labelling.					*
C*									*
C* Note that the colors can now be set or queried by name.  See IN_COLR	*
C* for details.								*
C*									*
C* If the line type is set to a single negative number, negative	*
C* values will have the line type specified and positive values		*
C* will be solid (line type = 1).  If the label is set to a single	*
C* number, say n, then every nth value will be labelled.  If label is	*
C* set to a negative value, -n, then every nth value will be labelled, 	*
C* but the line will not break for the label.				*
C*									*
C* The smoothing is a single value which specifies the amount of	*
C* smoothing to perform on the lines.  0 means that no smoothing will	*
C* occur, and is the default.						*
C*									*
C* The filter is a real value from 0 to 1 that specifies a filter for	*
C* the number of points along a line.					*
C*									*
C* IN_LINE ( LINE, VALUES, NEXP, ICOLOR, ITYPE, IWIDTH, ILABEL,		*
C*	     SMOOTH, FILTER, SCFLAG, IRET )				*
C*									*
C* Input parameters:							*
C*	LINE		CHAR*		LINE input			*
C*	VALUES (NEXP)	REAL		Data values to draw and label	*
C*	NEXP		INTEGER		Number expected			*
C*									*
C* Output parameters:							*
C*	ICOLOR (NEXP)	INTEGER		Color number array		*
C*	ITYPE  (NEXP)	INTEGER		Line type number array		*
C*	IWIDTH (NEXP)	INTEGER		Line width number array		*
C*	ILABEL (NEXP)	INTEGER		Line label number array		*
C*	SMOOTH		REAL		Line smoothing density		*
C*	FILTER		REAL		Line point filter		*
C*	SCFLAG		LOGICAL		Suppress small contour flag	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/GSC		06/90						*
C* S. Schotz/GSC	 7/90	Added negative contour feature, also	*
C*				reordered line input			*
C* M. desJardins/GSFC	 9/90	Fix error when no labels are requested	*
C* S. Schotz/GSC	 9/90	Can now query or set color name, call	*
C*				IN_COLR for color parsing		*
C* J. Whistler/SSAI	 4/92	Increased the size of group array	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* S. Jacobs/NCEP	 1/99	Added smoothing level to call		*
C* S. Jacobs/NCEP	 1/99	Allow negative label frequency		*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* S. Jacobs/NCEP	 5/99	Added line point filter			*
C* T. Lee/SAIC		12/01	Increased group string length to LLMXLN	*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	line
	INTEGER		icolor (*), itype (*), iwidth (*), ilabel (*)
	REAL		values (*)
C*
	CHARACTER	group (7)* (LLMXLN)
	LOGICAL		scflag
	INTEGER		iscflag
C------------------------------------------------------------------------
	iret = 0
	scflag = .false.
C
C*	Break list into four groups.
C
	CALL ST_CLST  ( line, '/', ' ', 7, group, ng, ier )
	DO  i = 1, 4
	    IF  ( group(i) .eq. ' ' )  group(i) = '1'
	END DO
	IF  ( group(5) .eq. ' ' )  group(5) = '0'
	IF  ( group(6) .eq. ' ' )  group(6) = '0'
	IF  ( group(7) .eq. ' ' )  group(7) = 'F'
C
C*      Decode color, type, width and label parts
C
	CALL IN_COLR ( group (1), nexp, icolor, ier )
        CALL ST_ILST ( group (2), ';', 0, nexp, itype,  ntyp, ier)
        CALL ST_ILST ( group (3), ';', 0, nexp, iwidth, nwid, ier)
        CALL ST_ILST ( group (4), ';', 0, nexp, ilabel, nlab, ier)
	CALL ST_NUMB ( group (5), ismoth, ier )
	CALL ST_CRNM ( group (6), filter, ier )
C
	CALL ST_LCUC ( group (7), group (7), ier )
	iscflag = INDEX ( group(7)(1:1), 'T' )
	IF ( iscflag .ne. 0 ) THEN
	    scflag = .true.
	END IF
C
	IF  ( ismoth .eq. 2 )  THEN
	    smooth = 5.0
	  ELSE IF  ( ismoth .eq. 1 )  THEN
	    smooth = 1.0
	  ELSE
	    smooth = 0.0
	END IF
C
C*	Set defaults for no entry.
C
	IF  ( ntyp .eq. 0 )  THEN
	    ntyp       = 1
	    itype  (1) = 1
	END IF
	IF  ( nwid .eq. 0 )  THEN
	    nwid       = 1
	    iwidth (1) = 1
	END IF
	IF  ( nlab .eq. 0 )  THEN
	    nlab       = 1
	    ilabel (1) = 1
	END IF
C
C*	Set the repeated patterns to fill buffer.
C
	IF  ( nwid .lt. nexp )  THEN
	    DO  i = nwid + 1, nexp
		j = MOD ( i-1, nwid ) + 1
		iwidth (i) = iwidth ( j )
	    END DO
	END IF
C
C*      If linetype is a single negative number, set positive numbers
C*      to solid and negative values to line type specified.  Otherwise
C*      repeat pattern to fill buffer if necessary.
C
	IF  ( (ntyp .eq. 1) .and. (itype(1) .lt. 0) ) THEN
	    jtype = - itype (1)
C
C*	    Check for positive or negative numbers.
C
	    DO  i = 1, nexp
	        IF  ( values (i) .ge. 0 )  THEN
		    itype (i) = 1
		  ELSE
		    itype (i) = jtype
		END IF
	    END DO
          ELSE
	    IF  ( ntyp .lt. nexp )  THEN
	        DO  i = ntyp + 1, nexp
		    itype  (i) = itype  ( MOD ( i-1, ntyp ) + 1 )
	        END DO
	    END IF
        END IF
C
C*      If label is a single number n, then label every nth contour.
C*      Otherwise repeat buffer to fill buffer if necessary.
C
	IF  ( ( nlab .eq. 1 ) .and.
     +	      ( ABS ( ilabel (1) ) .gt. 1 ) ) THEN
	    j = ABS ( ilabel (1) )
	    k = SIGN ( 1, ilabel (1) )
	    DO  i = 1, nexp
		IF  ( MOD ( i, j ) .eq. 1 )  THEN
		    ilabel (i) = 1 * k
		  ELSE
		    ilabel (i) = 0
		END IF
	    END DO
	  ELSE IF  ( nlab .eq. 1 )  THEN
	    j = ilabel (1)
	    DO  i = 1, nexp
		ilabel (i) = j
	    END DO
	  ELSE
	    IF  ( nlab .lt. nexp )  THEN
	        DO  i = nlab + 1, nexp
		    j = MOD ( i-1, nlab ) + 1
		    ilabel (i) = ilabel ( j )
	        END DO
	    END IF
        END IF
C*
	RETURN
	END
