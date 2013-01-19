	SUBROUTINE IDASH ( np, ix, iy, iret )
C************************************************************************
C* IDASH								*
C* 									*
C* This subroutine dashes line segments.				*
C* 									*
C* IDASH ( NP, IX, IY, IRET )						*
C* 									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	IX (NP)		INTEGER		X coordinates			*
C*	IY (NP)		INTEGER		Y coordinates			*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/87						*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/87	Cleaned up				*
C* M. desJardins/GSFC	 6/89	Retain dashing pattern between calls	*
C* S. Schotz/GSFC	 2/90	Added dot patterns			*
C* K. Brill/NMC		 9/91	Initialize NPT; fix dot case		*
C* M. desJardins/NMC	 1/93	Fix rounding errors when dashing lines	*
C* M. Linda/GSC		 6/96	Replaced local buffers with GBUFFT	*
C* I. Durham/GSC	 1/98   Removed saving of dash pattern from 	*
C*				common					*
C* S. Jacobs/NCEP	 3/98	Added line dashing scale factor - tszdsh*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GBUFFT.CMN'
C*
	INTEGER		ix (*), iy (*)
C
	LOGICAL		linflg, dotflg
C------------------------------------------------------------------------
	iret = NORMAL
	IF  ( np .le. 1 ) RETURN
C
C*	If the line type is a solid line, or hardware line type is used,
C*	send the line directly.
C
	IF  ( ( mltyp .eq. 1 ) .or. ( mlthw .eq. 2 ) ) THEN
	    CALL IDRAW ( np, ix, iy )
	    RETURN
	END IF
C
C*	Get segment information from common.
C
	kseg   = mseg
	dlen   = rmlen * tszdsh
	linflg = lnflg
	dotflg = dtflg
C
C*	Initialize number of points in buffer.
C
	npt = 0
C
C*	Move first point to buffer.
C
	IF  ( linflg .or. dotflg) THEN
	    jgx (1) = ix (1)
	    jgy (1) = iy (1)
	    npt = 1
	END IF
C
C*	Loop through the rest of the points.
C
	xb = FLOAT ( ix (1) )
	yb = FLOAT ( iy (1) )
	DO  i = 2, np
	    xa = xb
	    ya = yb
	    xb = FLOAT ( ix (i) )
	    yb = FLOAT ( iy (i) )
C
C*	    Compute distance to this point.
C
	    dist = SQRT ( ( xb - xa ) **2 + ( yb - ya ) **2 )
C
C*	    Loop until this line segment is used.
C
	    DO WHILE ( dist .gt. 0. )
C
C*	      Check whether this distance will fit in this line segment.
C
	      IF  ( dlen .gt. dist ) THEN
C
C*		Add entire line on odd segments; otherwise, leave space.
C
		IF  ( linflg ) THEN
		    npt = npt + 1
		    jgx (npt) = ix (i)
		    jgy (npt) = iy (i)
		END IF
		xa = ix (i)
		ya = iy (i)
C
C*		Reset length of current segment to be drawn.
C
		dlen = dlen - dist
		dist = 0.
C*
	       ELSE
C
C*		Find point to break line.
C
		xc = xa + dlen / dist * ( xb - xa )
		yc = ya + dlen / dist * ( yb - ya )
		adist = SQRT ( ( xc - xa ) **2 + ( yc - ya ) **2 )
C
		IF  ( dotflg ) THEN
C
C*                  Draw dot at end of space.
C
                    npt = 2
	            jgx (1) = NINT (xc)
 		    jgy (1) = NINT (yc)
                    jgx (2) = jgx (1)
		    jgy (2) = jgy (1)
		    CALL IDRAW  ( npt, jgx, jgy )
C
C*		    Reset variables.
C
		    IF  ( dlen .eq. dist ) THEN
		        dist = 0.
		      ELSE
		        dist = dist - adist
		    END IF
		    dotflg = .false.
                    linflg = .false.
                    dlen   = (-actpat(kseg)) * 0.5 * tszdsh
		    npt = 0
		    xa  = xc
		    ya  = yc
                  ELSE
C
C*		    Draw line on odd segment when linflg is set.  Otherwise,
C*		    leave space and start new line.
C
    		    IF  ( linflg ) THEN
		        npt = npt + 1
		        jgx ( npt ) = NINT ( xc )
		        jgy ( npt ) = NINT ( yc )
		        CALL IDRAW  ( npt, jgx, jgy )
		        npt = 0
		      ELSE
		        npt = 1
		        jgx ( npt ) = NINT ( xc )
		        jgy ( npt ) = NINT ( yc )
		    END IF
		    xa = xc
		    ya = yc
C
C*		    Start on next segment.
C
            	    kseg = kseg + 1
                    IF  ( linflg ) THEN
		       linflg = .false.
                      ELSE
                       linflg = .true.
                    END IF
            	    IF  ( kseg .gt. nseg ) THEN
		        kseg = 1
		        linflg = .true.
		    END IF
C*
		    IF  ( dlen .eq. dist ) THEN
		        dist = 0.
		      ELSE
		        dist = dist - adist
		    END IF
C
C*		    Compute new total length of segments.
C
	            dlen = actpat ( kseg ) * tszdsh
C
C*                  Check for dot pattern.
C
		    IF  ( dlen .lt. 0 ) THEN
			dotflg = .true.
			linflg = .false.
			dlen   = (-dlen) * 0.5
		    END IF
		END IF
	      END IF
	    END DO
	END DO
C
C*	Flush last buffer.  
C
	IF ( npt .gt. 1 ) CALL IDRAW ( npt, jgx, jgy )
C*
	RETURN
	END
