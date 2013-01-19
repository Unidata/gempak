	SUBROUTINE GDNREG ( ictyp, line, loci, info, iret )
C************************************************************************
C* GDNREG								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* T. Lee/SAIC		01/06	Copied from GPNREG			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1, rplypt(4)*24
	REAL		tmpt(2), xcrc(60), ycrc(60), xltln(2), yltln(2)
	REAL	 	xnew(2), ynew(2)
	REAL	 	smooth, linfltr
	LOGICAL		latlon, scflag
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
	    sys = 'N'
	    latlon = .false.
	    IF  ( loci(1:1) .eq. '@' )  THEN
		sys = 'G'
		loci = loci(2:)
	    ELSE IF  ( loci(1:1) .eq. '#' )  THEN
		sys = 'M'
		loci = loci(2:)
		latlon = .true.
	    END IF
C
C*	    Get point(s) for the plot.
C
	    CALL ST_RLST ( loci, ';', 0., 2, tmpt, num2, ier )
C
C*	    Separate the shape information.
C
	    CALL ST_CLST ( info, '/', ' ', 4, rplypt, num, ier )
C
C*	    Get the line type.
C
	    CALL ST_NUMB ( rplypt(1), ltype, ier )
	    IF  ( ltype .eq. IMISSD )  ltype = 1
C
C*	    Get the radius.
C
	    CALL ST_CRNM ( rplypt(2), rad, ier )
	    IF  ( rad .eq. RMISSD )  rad = 0.
C
C*	    Get the number of points on the perimeter.
C
	    CALL ST_NUMB ( rplypt(3), np, ier )
	    np = np + 1
	    IF  ( (np .gt. 55) .or. (np .le. 3) )  np = 55
C
C*	    Get the rotation.
C
	    CALL ST_CRNM ( rplypt(4), rot, ier )
	    IF  ( rot .eq. RMISSD )  rot = 0.
C
C*	    If using Normal coords, correct for PS device.
C
	    IF  ( sys .eq. 'N' )  THEN
		tmpt(1) = (xrght-xleft) * tmpt(1)
		tmpt(2) = (ytop -ybot ) * tmpt(2)
	    ENDIF
C
C*	    If using lat and lon coordinates convert to normal coords
C*	    since the proj may skew the object.
C
	    IF ( latlon ) THEN
	        xltln (1) = tmpt (1)	
	        yltln (1) = tmpt (2)	
	        xltln (2) = tmpt (1) + rad	
		yltln (2) = tmpt (2)
	        CALL GTRANS ( sys , 'N', 2, xltln, yltln, xnew,
     +			      ynew, ier )
		dx = xnew(2) - xnew(1)
		dy = ynew(2) - ynew(1)
		rad = SQRT ( dx*dx + dy*dy )
		tmpt(1) = xnew(1)
		tmpt(2) = ynew(1)
		sys = 'N'
	    END IF
C
C*	    Compute points around circumference.
C
	    ang = DTR * rot
	    xrad = rad * COS(ang)
	    yrad = rad * SIN(ang)
	    p    = TWOPI / ( np - 1 )
	    cosp = COS ( p )
	    sinp = SIN ( p )
	    xc   = xrad
	    yc   = yrad
	    xcrc (1) = tmpt(1) + xc
	    ycrc (1) = tmpt(2) + yc
C
	    DO  i = 1, np
		xcr = cosp * xc - sinp * yc
		ycr = sinp * xc + cosp * yc
		xc  = xcr
		yc  = ycr
		xcrc (i) = tmpt(1) + xc
		ycrc (i) = tmpt(2) + yc
	    END DO
C
C*	    Set attributes.
C
	    CALL GSCOLR ( icolor, ier )
	    CALL GSLINE ( ltype, 0, iwidth, 0, ier )
C
C*	    Plot polygon.
C
            IF ( ictyp .eq. 1 ) THEN
                np = np + 1
                xcrc (np) = xcrc (1)
                ycrc (np) = ycrc (1)
                CALL GLINE ( sys, np, xcrc, ycrc, ier )
            ELSE
                CALL GFILL ( sys, np, xcrc, ycrc, ier )
            END IF
C
	END IF
C*
	RETURN
	END
