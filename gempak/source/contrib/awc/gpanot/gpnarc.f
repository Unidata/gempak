	SUBROUTINE GPNARC ( ictyp, line, loci, info, ispcl, iret )
C************************************************************************
C* GPNARC								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* C. Bailey/HPC	10/06		Changes call to IN_LINE		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1, rplypt(5)*24
	INTEGER  	np
	REAL		tmpt(2), xcrc(60), ycrc(60)
        REAL		rlst(9), smooth, linfltr
	LOGICAL		scflag
C------------------------------------------------------------------------
C*	Get plot bounds in normal coordinates.
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
	    CALL ST_CLST ( info, '/', ' ', 5, rplypt, num, ier )
C
C*	    Get the line type.
C
	    CALL ST_CRNM ( rplypt(1), rtype, ier )
	    ltype = NINT ( rtype )
	    IF  ( ltype .eq. IMISSD )  ltype = 1
C
C*	    Get the radius in the specified coordinates.
C
	    CALL ST_CRNM ( rplypt(2), rad, ier )
	    IF  ( rad .eq. RMISSD )  rad = 0.
C
C*	    Get the the number of points to use for the arc.
C
	    CALL ST_NUMB ( rplypt(3), np,  ier )
	    np = np + 1
	    IF  ( (np .gt. 55) .or. (np .le. 3) )  np = 55
C
C*	    Get the starting angle.
C
	    CALL ST_CRNM ( rplypt(4), angstrt, ier )
	    IF  ( angstrt .eq. RMISSD )  angstrt = 0.
C
C*	    Get the ending angle.
C
	    CALL ST_CRNM ( rplypt(5), angend, ier )
	    IF  ( angend .eq. RMISSD )  angend = 360.
C
C*	    If using Normal coords, correct for PS device.
C
	    IF  ( sys .eq. 'N' )  THEN
		tmpt(1) = (xrght-xleft) * tmpt(1)
		tmpt(2) = (ytop -ybot ) * tmpt(2)
	    ENDIF
C
C*	    Compute arc coordinates.
C
	    ang1 = DTR * angstrt
	    ang2 = DTR * (angend-angstrt)
	    xrad = rad * COS(ang1)
	    yrad = rad * SIN(ang1)
	    p    = ang2 / ( np - 1 )
	    cosp = COS ( p )
	    sinp = SIN ( p )
	    xc   = xrad
	    yc   = yrad
	    xcrc (1) = tmpt(1)
	    ycrc (1) = tmpt(2)
	    xcrc (2) = tmpt(1) + xc
	    ycrc (2) = tmpt(2) + yc
C
C*	    Compute points around circumference.
C
	    np = np + 1
	    DO  i = 3, np
		xcr = cosp * xc - sinp * yc
		ycr = sinp * xc + cosp * yc
		xc  = xcr
		yc  = ycr
		xcrc (i) = tmpt(1) + xc
		ycrc (i) = tmpt(2) + yc
	    END DO
C
C*	    Set color.
C
	    CALL GSCOLR ( icolor, ier )

C
C*	    Plot arc.
C
	    IF ( ispcl .eq. 1 ) THEN
C
C*	        Set attributes.
C
	        CALL GSLINE ( ltype, 0, iwidth, 0, ier )
	        IF ( ictyp .eq. 3 ) THEN
		    np = np + 1
		    xcrc (np) = xcrc (1)
		    ycrc (np) = ycrc (1)
	            CALL GLINE ( sys, np, xcrc, ycrc, ier )
	        ELSE IF ( ictyp .eq. 2 ) THEN
	            CALL GFILL ( sys, np, xcrc, ycrc, ier )
	        ELSE 
                    np = np - 1
	            DO  i = 1, np
		        xcrc (i) = xcrc (i+1)
		        ycrc (i) = ycrc (i+1)
                    END DO
	            CALL GLINE ( sys, np, xcrc, ycrc, ier )
	        END IF
	    ELSE
C
C*	        Get the line type for the special line.
C
	        CALL ST_RLST ( info, '/', 0., 9, rlst, inum, ier )
	        itype = INT ( rlst(6) )
	        islstr = INT ( rlst(7) )
	        isldir = INT ( rlst(8) )
	        slsiz = rlst(9)
C
C*	        Set attributes.
C
	        CALL GSSPLN ( itype, islstr, isldir, slsiz, iwidth, ier)
	        IF ( ictyp .eq. 3 ) THEN
		    np = np + 1
		    xcrc (np) = xcrc (1)
		    ycrc (np) = ycrc (1)
	            CALL GSPLN ( sys, np, xcrc, ycrc, ier )
	        ELSE IF ( ictyp .eq. 2 ) THEN
	            CALL GFILL ( sys, np, xcrc, ycrc, ier )
	        ELSE 
                    np = np - 1
	            DO  i = 1, np
		        xcrc (i) = xcrc (i+1)
		        ycrc (i) = ycrc (i+1)
                    END DO
	            CALL GSPLN ( sys, np, xcrc, ycrc, ier )
	        END IF
	    END IF
	END IF
C*
	iret = 0
C*
	RETURN
	END
