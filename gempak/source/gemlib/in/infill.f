	SUBROUTINE IN_FILL  ( fint, fline, gmin, gmax, fval, nflvl,
     +                        rfint, fmin, fmax, ifcolr, ifltyp,
     +			      iflabl, iret )
C************************************************************************
C* IN_FILL								*
C*									*
C* This subroutine processes the contour fill specification.  FINT is	*
C* the contour fill values. FLINE is the contour fill colors and types.	*
C* GMIN and GMAX are input as the mininum and maximum values in the	*
C* grid. The list of fill levels is returned in FVAL, along with FMIN	*
C* and FMAX, the minimum and maximum to be filled. The list of fill 	*
C* colors and types is returned in IFCOLR and IFLTYP. Fill label, 	*
C* IFLABL is not used. For loops, this routine is called only once for 	*
C* the first pixmap.							*
C* 									*
C* FINT is expected to be of the form:					*
C*									*
C*	    increment/minimum/maximum					*
C*    or								*
C*          value1;value2;...;valueN					*
C*									*
C* where the minimum and maximum give the range for the contours. If	*
C* the minimum equals the maximum, a single contour with that value	*
C* is assumed.  In the latter specification, the specified contour	*
C* levels are used, and the minimum and maximum are ignored.		*
C*									*
C* FLINE is expected to be of the form:					*
C*									*
C*	    col1;col2;.../typ1;typ2...					*
C*									*
C* The fill type can be 1 (solid), 2 (slanted dash) or 3 (slanted 	*
C* line). If the fill type is set to a single negative number,		*
C* negative values will have the fill type specified and positive	*
C* values will be solid (fill type = 1). If the fill type is set to 0,	*
C* a default value of 1 will be used.					*
C*									*
C* IN_FILL  ( FINT, FLINE, GMIN, GMAX, FVAL, NFLVL, RFINT, FMIN, FMAX,	*
C*	      IFCOLR, IFLTYP, IFLABL, IRET )				*
C*									*
C* Input parameters:							*
C*	FINT		CHAR*		Input for fill contours		*
C*	FLINE		CHAR*		Input for fill colors/types	*
C*	GMIN		REAL		Minimum grid value		*
C*	GMAX		REAL		Maximum grid value		*
C*									*
C* Output parameters:							*
C*	FVAL (*)	REAL		List of fill levels		*
C*	NFLVL		INTEGER		Number of fill levels		*
C*	RFINT		REAL		Calculated fill interval	*
C*	FMIN		REAL		Fill minimum			*
C*	FMAX		REAL		Fill maximum			*
C*	IFCOLR (*)	INTEGER		Color number array		*
C*	IFLTYP (*)	INTEGER		Fill type number array		*
C*	IFLABL (*)	INTEGER		Fill label number array		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid input		*
C**									*
C* Log:									*
C* T. Lee/SAIC		11/01						*
C* T. Lee/SAIC		12/01	Increased substr string length to LLMXLN*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	fint, fline
	REAL		fval (*)
	INTEGER		ifcolr (*), ifltyp (*), iflabl (*)
C*
	CHARACTER	group (2) * (LLMXLN), substr (3) * (LLMXLN)
C*
	LOGICAL		onelev
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	nflvl = 0
C
C*	Break FINT into three substrings.
C
	CALL ST_CLST ( fint, '/', ' ', 3, substr, nst, ier )
C
C*	Look at the first substring to see if it contains a list of
C*	values or a single number giving the increment.
C
	CALL ST_RLST ( substr (1), ';', 0.0, LLCLEV, fval, nflvl, ier )
C
C*	Input is a list of contour levels
C
	IF ( nflvl .gt. 1 ) THEN
	    rfint = 0.
	    fmin  = 0.
	    fmax  = 0.
	  ELSE
C
C*	    Contour levels have not been listed.
C*	    Decode range from second and third substrings.
C
	    CALL ST_CRNM ( substr (2), fmin, ier )
	    CALL ST_CRNM ( substr (3), fmax, ier )
C
C*	    Check for single contour level.
C
	    IF ( ( fmin .eq. fmax ) .and. .not. ERMISS ( fmin ) ) THEN
		fval (1) = fmin
		rfint = 0.
		nflvl = 1
	      ELSE
C
C*		Generate multiple contour levels.
C
		rint = fval (1)
		CALL GR_CLVL  ( LLCLEV, fmin, fmax, rint, gmin, gmax,
     +				nflvl, fval, rfint, iret )
		IF ( iret .ne. 0 )  THEN
		    nflvl = 0
		    rfint = 0.
		    CALL ER_WMSG ( 'GR', iret, ' ', ier )
		    iret = -3
		    RETURN
		END IF
	    END IF
	END IF
C
C*	Break FLINE into two groups.
C
	CALL ST_CLST  ( fline, '/', ' ', 2, group, ng, ier )
	DO  i = 1, 2
	    IF  ( group(i) .eq. ' ' )  group(i) = '1'
	END DO
C
	IF  ( nflvl .eq. LLCLEV )  THEN
	    nflvl = nflvl - 1
	END IF
C
C*	Decode color and type parts.
C
	nflvl1 = nflvl + 1
	CALL IN_COLR ( group (1), nflvl1, ifcolr, ier )
	CALL ST_ILST ( group (2), ';', 1, nflvl1, ifltyp, ntyp, ier)
C
C*	Set defaults and for no entry.
C
	DO  i = 1, nflvl1
	    iflabl (i) = 1
	END DO
C
	IF  ( ntyp .eq. 0 )  THEN
	    ntyp       = 1
	    ifltyp (1) = 1
	  ELSE
C
C*	    Set the repeated patthern to fill types.
C
	    jfltyp = ifltyp (1)
	    IF ( ntyp .lt. nflvl1 )  THEN
		DO i = ntyp + 1, nflvl1
		    ifltyp (i) = ifltyp ( MOD (i-1, ntyp) + 1 )
		END DO
	    END IF
	END IF
C
C*	Sort the levels from smallest to largest.
C
        IF  ( nflvl .gt. 0 )  THEN
	    DO  i = 1, nflvl - 1
		DO  j = i+1, nflvl
		    IF  ( fval (i) .gt. fval (j) )  THEN
			jcol = ifcolr (i)
			jtyp = ifltyp (i)
			fsav = fval (i)
			ifcolr (i) = ifcolr (j)
			ifltyp (i) = ifltyp (j)
			fval   (i) = fval   (j)
			ifcolr (j) = jcol
			ifltyp (j) = jtyp
			fval   (j) = fsav
		    END IF
		END DO
	    END DO
C
C*	    Check that at least one line has a color.  
C
	    onelev = .false.
	    DO  i = 1, nflvl1
		IF  ( ifcolr (i) .gt. 0 )  onelev = .true.
	    END DO
C
	    IF  ( .not. onelev )  THEN
		nflvl = 0
		RETURN
	      ELSE
C
C*		Eliminate duplicate fill levels.
C
		ilvl = 1
		DO  i = 2, nflvl
		    IF  ( fval (i) .ne. fval (i-1) )  THEN
			ilvl = ilvl + 1
			fval (ilvl) = fval (i)
			ifcolr (ilvl) = ifcolr (i)
			ifltyp (ilvl) = ifltyp (i)
		    END IF
		END DO
		nflvl  = ilvl
		nflvl1 = ilvl + 1
	    END IF
	END IF
C
C*	If fill type is a single negative number, sort the fill
C*	values, and set positive values to solid and negative 
C*	values to fill type specified.
C
	IF  ( ( ntyp .eq. 1 ) .and. ( jfltyp .lt. 0 ) ) THEN
	    jtype = - jfltyp
C
C*	    Check for positive or negative values.
C
	    IF  ( fval (1) .le. 0. )  THEN
		ifltyp (1) = jtype
	      ELSE
		ifltyp (1) = 1
	    END IF
C
	    DO  i = nflvl, 1, -1
		IF  ( fval (i) .ge. 0. )  THEN
		    ifltyp (i+1) = 1
		  ELSE 
		    ifltyp (i+1) = jtype
		END IF
	    END DO
C
	END IF
C*
	RETURN
	END
