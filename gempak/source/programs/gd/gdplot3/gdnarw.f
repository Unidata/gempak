	SUBROUTINE GDNARW ( ictyp, line, loci, info, iret )
C************************************************************************
C* GDNARW								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* T. Lee/SAIC		01/06	Copied from GPNARW			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( HANGLE1 = DTR * 30.0 )
	PARAMETER	( HANGLE2 = DTR * 10.0 )
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1, arwpts(6)*24
	REAL		tmpt(2), xarw(2), yarw(2), xfarw(8), yfarw(8)
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
     +                 smooth, linfltr, scflag, ier )
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
	    CALL ST_RLST ( loci, ';', 0., 2, tmpt, num2, ier )
C
C*	    Separate the shape information.
C
	    CALL ST_CLST ( info, '/', ' ', 6, arwpts, num, ier )
C
C*	    Get the line type.
C
	    CALL ST_NUMB ( arwpts(1), ltype, ier )
	    IF  ( ltype .eq. IMISSD )  ltype = 1
C
C*	    Get the arrow type.
C
	    CALL ST_NUMB ( arwpts(2), iatp, ier )
	    IF  ( iatp .eq. IMISSD )  iatp = 1
C
C*	    Get the arrow size.
C
	    CALL ST_CRNM ( arwpts(3), asize, ier )
	    IF  ( asize .eq. RMISSD )  asize = 1.
C
C*	    Get the head size.
C
	    CALL ST_CRNM ( arwpts(4), hsize, ier )
	    IF  ( hsize .eq. RMISSD )  hsize = 1.
C
C*	    Get the head angle multiplier.
C
	    CALL ST_CRNM ( arwpts(5), hdang, ier )
	    IF  ( hdang .eq. RMISSD )  hdang = 1.
C
C*	    Get the rotation angle.
C
	    CALL ST_CRNM ( arwpts(6), rot, ier )
	    IF  ( rot .eq. RMISSD )  rot = 0.
C
C*	    If using Normal coords, correct for PS device.
C
	    IF  ( sys .eq. 'N' )  THEN
		tmpt(1) = (xrght-xleft) * tmpt(1)
		tmpt(2) = (ytop -ybot ) * tmpt(2)
	    ENDIF
C
C*	    Set attributes.
C
	    CALL GSLINE ( ltype, 0, iwidth, 0, ier )
	    CALL GSCOLR ( icolor, ier )
C
C*	    Find arrow length, head size and head angles.
C
	    arrwln = asize * .1
	    hsize  = hsize * .025
	    rdir   = rot * DTR
	    rang1  = rdir - ( HANGLE1 * hdang )
	    rang2  = rdir + ( HANGLE1 * hdang )
	    rang3  = rdir - ( HANGLE2 * hdang )
	    rang4  = rdir + ( HANGLE2 * hdang )
C
C*	    Choose arrow type. Type 1 is similar to the original 
C*	    GEMPAK arrow. Type 2 is a solid arrow, which can be
C*	    filled or open.
C
	    IF  ( iatp .eq. 2 )  THEN
C
C*		Find the 7 points that make a filled arrow.
C
		xfarw(1) = tmpt(1)
		yfarw(1) = tmpt(2)
		xfarw(2) = ( hsize * COS(rang1) ) + tmpt(1)
		yfarw(2) = ( hsize * SIN(rang1) ) + tmpt(2)
		xfarw(3) = ( hsize * .75 * COS(rang3) ) + tmpt(1)
		yfarw(3) = ( hsize * .75 * SIN(rang3) ) + tmpt(2)
		xfarw(4) = ( arrwln * COS(rdir) ) + xfarw(3)
		yfarw(4) = ( arrwln * SIN(rdir) ) + yfarw(3)
		xfarw(7) = ( hsize * COS(rang2) ) + tmpt(1)
		yfarw(7) = ( hsize * SIN(rang2) ) + tmpt(2)
		xfarw(6) = ( hsize * .75 * COS(rang4) ) + tmpt(1)
		yfarw(6) = ( hsize * .75 * SIN(rang4) ) + tmpt(2)
		xfarw(5) = ( arrwln * COS(rdir) ) + xfarw(6)
		yfarw(5) = ( arrwln * SIN(rdir) ) + yfarw(6)
                IF ( ictyp .eq. 1 ) THEN
                    xfarw (8) = xfarw (1)
                    yfarw (8) = yfarw (1)
                    CALL GLINE ( sys, 8, xfarw, yfarw, ier )
                ELSE
                    CALL GFILL ( sys, 7, xfarw, yfarw, ier )
                END IF
C
	    ELSE
C
C*		Draw the arrow body.
C
		xarw(1) = tmpt(1)
		yarw(1) = tmpt(2)
		xarw(2) = ( arrwln * COS(rdir) ) + tmpt(1)
		yarw(2) = ( arrwln * SIN(rdir) ) + tmpt(2)
		CALL GLINE ( sys, 2, xarw, yarw, ier )
C
C*		Draw the arrow head.
C
		CALL GSLINE ( 1, 0, iwidth, 0, ier )
		xarw(2) = tmpt(1) + ( hsize * COS(rang1) )
		yarw(2) = tmpt(2) + ( hsize * SIN(rang1) )
		CALL GLINE ( sys, 2, xarw, yarw, ier )
C
		xarw(2) = tmpt(1) + ( hsize * COS(rang2) )
		yarw(2) = tmpt(2) + ( hsize * SIN(rang2) )
		CALL GLINE ( sys, 2, xarw, yarw, ier )
C
	    END IF
C
	END IF
C*
	RETURN
	END
