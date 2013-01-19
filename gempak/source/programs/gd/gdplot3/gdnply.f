	SUBROUTINE GDNPLY ( ictyp, line, loci, info, iret )
C************************************************************************
C* GDNPLY								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* T. Lee/SAIC		01/06	Copied from GPNPLY			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1, plypts(20)*24
	REAL		tmpt(2), xplypt(20), yplypt(20)
	REAL		smooth, linfltr
	LOGICAL		scflag
C------------------------------------------------------------------------
C*	Get plot bounds in Normal coordinates.
C
        CALL GQBND ( 'N', xleft, ybot, xrght, ytop, ier )
C
C*	Parse the fill input.
C
	CALL IN_LINE ( line, 1, 1, icolor, itype, iwidth, ilabel, 
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
	    ELSE IF  ( loci(1:1) .eq. '#' )  THEN
		sys = 'M'
		loci = loci(2:)
	    END IF
C
C*	    Get the point(s) for the plot.
C
	    CALL ST_CLST ( loci, '/', ' ', 20, plypts, num, ier )
C
C*	    Get the line type.
C
	    CALL ST_NUMB ( info, ltype, ier )
C
	    DO  i = 1, num
		CALL ST_RLST ( plypts(i), ';', 0., 2, tmpt, num2,
     +			       ier )
		xplypt(i) = tmpt(1)
		yplypt(i) = tmpt(2)
C
C*		If using Normal coords, correct for PS device.
C
		IF  ( sys .eq. 'N' )  THEN
		    xplypt(i) = (xrght-xleft) * xplypt(i)
		    yplypt(i) = (ytop -ybot ) * yplypt(i)
		END IF
C
	    END DO
C
C*	    Set attributes.
C
	    CALL GSCOLR  ( icolor, ier )
	    CALL GSLINE  ( ltype, 0, iwidth, 0, ier )
C
C*	    Plot polygon.
C
            IF ( ictyp .eq. 1 ) THEN
                num = num + 1
                xplypt (num) = xplypt (1)
                yplypt (num) = yplypt (1)
                CALL GLINE ( sys, num, xplypt, yplypt, ier )
            ELSE
                CALL GFILL ( sys, num, xplypt, yplypt, ier )
            END IF
C
	END IF
C*
	RETURN
	END
