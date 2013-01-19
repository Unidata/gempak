	SUBROUTINE GDNLIN ( ictyp, line, loci, info, iltyp, iret )
C************************************************************************
C* GDNLIN								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* T. Lee/SAIC		01/06	Copied from GPNLIN			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1, linpts(20)*24
	REAL		tmpt(2), xlin(20), ylin(20)
        REAL		rlst(5)
        REAL		smooth, linfltr
	LOGICAL		scflag
C------------------------------------------------------------------------
C*	Get plot bounds in Normal coordinates.
C
        CALL GQBND ( 'N', xleft, ybot, xrght, ytop, ier )
C
C*	Parse the fill input.
C
	CALL IN_LINE ( line, svalue, 1, icolor, jtype, iwidth, ilabel,
     +		       smooth, linfltr, scflag, ier )
C
	IF  ( icolor .ne. 0 )  THEN
C
C*	    Set coordinate system to be used.
C
	    sys = 'N'
	    IF  ( loci(1:1) .eq. '@' )  THEN
		sys = 'G'
		loci = loci(2:)
	    ELSE IF  ( loci(1:1) .eq. '#' )  THEN
		sys = 'M'
		loci = loci(2:)
	    END IF
C
C*	    Get the point(s) for the plot.
C
	    CALL ST_CLST ( loci, '/', ' ', 20, linpts, num, ier )
C
C*	    Get the line type.
C
	    CALL ST_RLST ( info, '/', 0., 4, rlst, inum, ier )
	    itype = INT ( rlst(1) )
	    islstr = INT ( rlst(2) )
	    isldir = INT ( rlst(3) )
	    slsiz = rlst(4)
C
	    DO  i = 1, num
		CALL ST_RLST ( linpts(i), ';', 0., 2, tmpt, num2, ier )
C
C*              If using Normal coords, correct for PS device.
C
		IF  ( sys .eq. 'N' )  THEN
		    tmpt(1) = (xrght-xleft) * tmpt(1)
		    tmpt(2) = (ytop -ybot ) * tmpt(2)
		END IF
C
		xlin(i) = tmpt(1)
		ylin(i) = tmpt(2)
	    END DO
C
C*	    Set attributes.
C
	    CALL GSCOLR ( icolor, ier )
	    IF ( iltyp .eq. 1 ) THEN
	      CALL GSLINE ( itype, 0, iwidth, 0, ier )
C
C*	      Plot line.
C
	      CALL GLINE  ( sys, num, xlin, ylin, ier )
	    ELSE IF ( iltyp .eq. 2 ) THEN
	      CALL GSSPLN ( itype, islstr, isldir, slsiz, iwidth, ier )
C
C*	      Plot line.
C
	      CALL GSPLN  ( sys, num, xlin, ylin, ier )
	    END IF
C
	END IF
C*
	RETURN
	END
