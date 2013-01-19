	SUBROUTINE IN_CINT ( cint, grid, npts, gmin, gmax, cval, nv,
     +			     clbl, rint, iret )
C************************************************************************
C* IN_CINT								*
C*									*
C* This subroutine processes the user contour specification.  If	*
C* GMIN or GMAX is set to the missing value, the values in the grid	*
C* are used to compute GMIN and GMAX.  GRID is not used if GMIN and	*
C* GMAX are not missing.  An array of values is returned.		*
C* If a number-of-digits, ndig, is specified, the subroutine will pad	*
C* the beginning of the integer part of the label with enough zeroes	*
C* to expand that part of the label to have ndig characters, not	*
C* including the minus sign, if any.					*
C*									*
C* CINT is expected to be of the form:					*
C*									*
C*	    increment/minimum/maximum/ndigits				*
C*	or								*
C*	    value1;value2;...;valueN///ndigits				*
C*	or								*
C*	    value1=label1;value2=label2;...;valueN=labelN		*
C*									*
C* where the minimum and maximum give the range for the contours. If	*
C* the minimum equals the maximum, a single contour with that value	*
C* is assumed.  In the latter specification, the specified contour	*
C* levels are used, and the minimum and maximum are ignored.		*
C* The fourth parameter, ndigits, forces all labels with fewer than     *
C* ndigits digits in the integer portion of the label to expand to      *
C* ndigits by padding the leading digit with zeroes.                    *
C*									*
C* IN_CINT ( CINT, GRID, NPTS, GMIN, GMAX, CVAL, NV, CLBL, RINT, IRET )	*
C*									*
C* Input parameters:							*
C*	CINT		CHAR*		Input for contours		*
C*	GRID (NPTS)	REAL		Array of data			*
C*	NPTS		INTEGER		Number of grid values		*
C*									*
C* Input and output parameters:						*
C*	GMIN		REAL		Minimum grid value		*
C*	GMAX		REAL		Maximum grid value		*
C*									*
C* Output parameters:							*
C*	CVAL (*)	REAL		List of contour levels		*
C*	NV		INTEGER		Number of contour levels	*
C*	CLBL (*)	CHAR*		Contour labels			*
C*	RINT		REAL		Calculated contour interval	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid input		*
C*					 -4 = constant grid		*
C**									*
C* Log:									*
C* K. Brill/GSC		05/90						*
C* G. Krueger/EAI	07/93	Modified to return RINT			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* T. Lee/SAIC		12/01	Increased substr string length to LLMXLN*
C* C. Bailey/HPC	 6/06	Modified to return contour labels	*
C* S. Jacobs/NCEP	11/06	Fixed bug when min=max			*
C* R. Jones/NCEP	12/06	Added number of digits specification	*
C* S. Schotz/NCEP       2/07    Correct case where no cint is set      
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cint
	REAL		grid (*), cval (*)
	CHARACTER*(*)	clbl (*)
C*
	CHARACTER	substr (4) * (LLMXLN), cname (LLCLEV)*24,
     +			carr(2)*24
        CHARACTER       clab1*24, clab2*24
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	nv = 0
	num = 0
C
C*	Break CINT into 4 substrings.
C
	CALL ST_CLST ( cint, '/', ' ', 4, substr, nst, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -3
	    CALL ER_WMSG ( 'IN', iret, cint, ier )
	    RETURN
	END IF
C
C*	Convert the fourth item to an integer. Check for valid values
C*	for the number of leading zeroes to be added to a data value.
C
	CALL ST_NUMB ( substr (4), npad, ier )
	IF  ( ( npad .lt. 0 ) .or. ( npad .gt. 9 ) )  npad = 0
C
C*	Look at the first substring to see if it contains a list of
C*	values or a single number giving the increment.
C
	CALL ST_CLST ( substr (1), ';', ' ', LLCLEV, cname, nv, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -3
	    CALL ER_WMSG ( 'IN', iret, cint, ier )
	END IF
C
C	Check for case where no contour interval is specified
C
	IF (nv .eq. 0) THEN
C
C	   Initilize contour interval when not specified
C
	   cval(1) = 0.0 
	ELSE
C
C*	   Loop through each contour value 
C
	   DO i = 1, nv
C
C*	      Separate contour value from label if appropriate 
C
	      CALL ST_CLST ( cname (i), '=', ' ', 2, carr, num, ier )
C
C*	      Convert contour value string into a real number
C
       	      CALL ST_CRNM(carr(1), rnum, ier )
	      cval(i) = rnum
C
C*	      Check to see if contour label specified with value.
C*            If not, use contour value as label
C
	      IF (num .gt. 1) then
		clbl(i) = carr(2)
	      ELSE
		clbl(i) = carr(1)
	      END IF
	  END DO
	END IF
C
        IF ( nv .gt. 1 ) THEN
	    rcint = 0.0
	    cmin = 0.0
	    cmax = 0.0
	  ELSE
C
C*	    Contour levels have not been listed.
C*	    Decode range from second and third substrings.
C
	    CALL ST_CRNM ( substr (2), cmin, ier )
	    CALL ST_CRNM ( substr (3), cmax, ier )
C
C*	    Check for single contour level.
C
	    IF ( cmin .eq. cmax .and. .not. ERMISS ( cmin ) ) THEN
C 
C*              Check if label has been specified, if not set cval(1) 
C*              and contour label to cmin
C
	        cval (1) = cmin
		IF (num .le. 1) then
		    CALL GR_LABL(cval(1), 0, 0, clbl(1), nchar, ier)
		END IF    
		nv   = 1
	      ELSE
C
C*		Generate multiple contour levels.
C
		IF ( ERMISS ( gmin ) .or. ERMISS ( gmax ) ) THEN
C
C*		    Find grid maximum and minimum values.
C
		    gmin =  1.E31
		    gmax = -1.E31
		    DO i = 1, npts
			IF ( .not. ERMISS ( grid (i) ) ) THEN
			  IF ( grid (i) .lt. gmin ) gmin = grid (i)
			  IF ( grid (i) .gt. gmax ) gmax = grid (i)
			END IF
		    END DO
		    IF ( gmin .eq. 1.e31 .or. gmin .eq. gmax ) THEN
			iret = -4
			CALL ER_WMSG ( 'IN', iret, ' ', ier )
			RETURN
		    END IF
	        END IF
C
C*		Specified contour label strings not allowed.
C
		rcint = cval (1)
		CALL GR_CLVL ( LLCLEV, cmin, cmax, rcint, gmin, gmax,
     +				nv, cval, rint, iret )
		IF ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'GR', iret, ' ', ier )
		    iret = -3
		    CALL ER_WMSG ( 'IN', iret, ' ', ier )
		    RETURN
		END IF
C
C*		Convert contour values to strings
C
		DO i = 1, nv
		    CALL GR_LABL(cval(i), 0, 0, clbl(i), nchar, ier)
		END DO
	    END IF
	END IF
C
C*	Add zero-padding to the contour values.
C
	IF  ( npad .gt. 0 )  THEN
	    DO  ii = 1, nv
		CALL ST_NULL ( clbl(ii), clab1, lenc, ier )
		CALL CST_ZPAD ( clab1, npad, clab2, ier )
		CALL ST_RNUL ( clab2, clbl(ii), lenc, ier )
	    END DO
	END IF
C*
	RETURN
	END
