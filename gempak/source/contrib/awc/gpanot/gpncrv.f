	SUBROUTINE GPNCRV ( ictyp, line, loci, info, iret )
C************************************************************************
C* GPNCRV								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* T. Lee/SAIC		01/06	Minor bug fix in "info" parsing		*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NCPTS = 100 )
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1, crvpts(20)*24, cvinfo(2)*24
	REAL		tmpt(2), xcrv(20), ycrv(20), xval(20),
     +			yval(20), xout(NCPTS), yout(NCPTS)
	REAL		smooth, linfltr
	LOGICAL		scflag
C------------------------------------------------------------------------
C*	Get plot bounds in Normal coordinates.
C
        CALL GQBND ( 'N', xleft, ybot, xrght, ytop, ier )
C
C*	Parse the fill input.
C
	CALL IN_LINE ( line, 1, 1, icolor, jtype, iwidth, ilabel, 
     +		       smooth, linfltr, scflag, ier )
C
	IF  ( icolor .ne. 0 )  THEN
C
C*	    Set the coordinate system to be used.
C
	    sys = 'N'
	    IF  ( loci(1:1) .eq. '@' )  THEN
		sys = 'G'
		loci = loci(2:)
	    ELSE IF  ( loci(1:1) .eq. '#' ) THEN
		sys = 'M'
		loci = loci(2:)
	    END IF
C
C*	    Get the point(s) for the plot.
C
	    CALL ST_CLST ( loci, '/', ' ', 20, crvpts, num, ier )
C
C*	    Separate the shape information.
C
	    CALL ST_CLST ( info, '/', ' ', 2, cvinfo, num1, ier )
C
C*	    Get the line type.
C
	    CALL ST_NUMB ( crvpts(1), itype, ier )
	    IF  ( itype .eq. IMISSD )  itype = 1
C
C*	    Get the curve type.
C
	    CALL ST_NUMB ( crvpts(2), ictyp, ier )
	    IF  ( ictyp .eq. IMISSD )  ictyp = 1
C
C*	    Get the user points along the curve.
C
	    DO  i = 1, num
		CALL ST_RLST ( crvpts(i), ';', 0., 2, tmpt, num2, ier )
C
C*		If using Normal coords, correct for PS device.
C
		IF  ( sys .eq. 'N' )  THEN
		    tmpt(1) = (xrght-xleft) * tmpt(1)
		    tmpt(2) = (ytop -ybot ) * tmpt(2)
		END IF
C
		xcrv(i) = tmpt(1)
		ycrv(i) = tmpt(2)
	    END DO
C
C*	    Transform to Normal coords in order to compute a smooth 
C*	    curve. NCPTS defines how many points to compute on the 
C*	    curve.
C
	    CALL GTRANS ( sys, 'N', num, xcrv, ycrv, xval, yval,
     +			  ier )
	    xmin = 1e+8
	    xmax = 1e-8
	    DO  j = 1, num
		xmin = AMIN1 ( xmin, xval(j) )
		xmax = AMAX1 ( xmax, xval(j) )
	    END DO
	    DO  k = 1, NCPTS
		xout(k) = xmin + (k-1)*(xmax-xmin) / (NCPTS-1)
	    END DO
C
C*	    Calculate NCPTS points on the curve.
C
	    CALL GCURVE ( ictyp, num, xval, yval, NCPTS, xout, yout,
     +			  ier )
C
C*	    Set attributes.
C
	    CALL GSCOLR ( icolor, ier )
	    CALL GSLINE ( itype, 0, iwidth, 0, ier )
C
C*	    Plot curve.
C
	    CALL GLINE  ( 'N', NCPTS, xout, yout, ier )
	END IF
C*
	iret = 0
C*
	RETURN
	END
