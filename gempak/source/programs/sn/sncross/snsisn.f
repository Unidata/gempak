	SUBROUTINE SNSISN  ( line, x, pontha, ybot, ytop, ithinc, thmin,
     +			    thmax, nstn, sloc, pdat, iret )
C************************************************************************
C* SNSISN								*
C*									*
C* This subroutine draws isentropes for the cross section program.	*
C*									*
C* SNSISN  ( LINE, X, PONTHA, YBOT, YTOP, ITHINC, THMIN, THMAX,		*
C*           NSTN, SLOC, PDAT, IRET )					*
C*									*
C* Input parameters:							*
C*	LINE		CHAR*		Contour color/dash/width/label	*
C*	X (LLMAXD)	REAL		Grid locations			*
C*	PONTHA(LLMAXD,*)REAL		Pressure on isentropes		*
C*	YBOT		REAL		Maximum pressure plotted	*
C*	YTOP		REAL		Minimum pressure plotted	*
C*	ITHINC		INTEGER		Theta interval			*
C*	THMIN		REAL		Minimum theta			*
C*	THMAX		REAL		Maximum theta			*
C*	NSTN		INTEGER		Number of stations		*
C*	SLOC (NSTN)	REAL		Location of stations		*
C*	PDAT (LLTMCX,*)	REAL		Pressure on isentropes at stns	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* G. Huffman/GSC	11/88	Doc, LINE, LBLFRQ,MARKER,labelling	*
C* M. desJardins/GSFC	 8/89	Use 0 for hardware lines		*
C* S. Schotz/GSC	 6/90	Call IN_LINE now, eliminate markers	*
C* S. Schotz/GSC	 7/90	Update call to IN_LINE			*
C* M. desJardins/GSFC	 3/91	Add LLTMCX				*
C* K. Brill/NMC		12/91	Allow multiple line characteristics	*
C* K. Brill/NMC		01/92	Check for too many contours		*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line
	REAL		x (*), pontha (LLMAXD,*),
     +			sloc (*), pdat (LLTMCX,*)
C*
	CHARACTER	label*10
	REAL		xx (LLMAXD+2), yy (LLMAXD+2), value (LLCLEV)
	LOGICAL		above, done, scflag
	INTEGER		isncol (LLCLEV), isndsh (LLCLEV),
     +			isnlbl (LLCLEV), isnwid (LLCLEV)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	thinc = ithinc
C
C*	Decode user input for isentrope color/dash pattern/width,
C*	and label interval
C
	klvc = ( thmax - thmin ) / thinc + 1.0
	IF ( klvc .gt. LLCLEV ) klvc = LLCLEV
	DO il = 1, klvc
	    value (il) = 0.
	END DO
	CALL IN_LINE ( line, value, klvc, isncol, isndsh, isnwid,
     +		       isnlbl, smth, fltr, scflag, ier )
C
C*	Loop through isentropic lines.
C
	IF  ( isncol (1) .ne. 0 ) THEN
C
C*	  Save line type.
C
	  CALL GQLINE ( il1, il2, il3, il4, ier )
C
C*	  Loop through all the isentropes.
C
	  curtha = thmin
	  lev    = 2
	  above  = .false.
	  ilc    = 1
	  DO WHILE ( curtha .le. thmax .and. ilc .le. klvc )
C
C*	    Set color and line characteristics.
C
	    CALL GSCOLR ( isncol (ilc), ier )
	    CALL GSLINE ( isndsh (ilc), 0, isnwid (ilc), 0, ier )
C
C*	    Check to see if data is all above surface.
C
	    IF  ( .not. above ) THEN
		above = .true.
		knt   = 0
		DO  i = 1, LLMAXD
		    p = pontha (i,lev)
		    IF (( .not. ERMISS ( p )) .and. 
     +                           (p .le. pontha (i,1))) THEN
C
C*			If this is a first point above ground, intersect
C*			surface at previous point.
C
			IF ( (knt .eq. 0) .and. (i .gt. 1) .and.
     +			   ( .not. ERMISS (pontha (i-1,1) ) ) ) THEN
			    CALL SNSINS ( x(i-1), x(i), pontha(i-1,lev),
     +					  p, pontha (i-1,1),
     +                                    pontha (i,1),
     +					  xint, yint, ier )
			    knt = 1
			    xx (knt) = xint
			    yy (knt) = yint
			END IF
C
C*			Add this point to the buffer.
C
			knt = knt + 1
			xx (knt) = x (i)
			yy (knt) = pontha (i,lev)
		      ELSE
C
C*			Data is still not all above the surface.
C*			Plot buffered points if any.  Make sure last
C*			point intersects the surface line.
C
			above = .false.
			IF  ( knt .gt. 1 )  THEN
C
C*			    Find intersection with surface after last
C*			    point.
C
			    IF ( .not. ERMISS (pontha (i,lev) ) ) THEN
			      CALL SNSINS ( x(i-1), x(i),
     +                                    pontha (i-1,lev),
     +					  p, pontha (i-1,1),
     +                                    pontha (i,1),
     +					  xint, yint, ier )
			      knt = knt + 1
			      xx (knt) = xint 
			      yy (knt) = yint
			    END IF
			    CALL GLINE ('M', knt, xx, yy, ier )
C*
			END IF
			knt = 0
		    END IF
		END DO
C
C*		Flush last buffer.
C
		IF (knt .gt. 1) CALL GLINE ('M', knt, xx, yy, ier )
C
C*		Data is all above the surface.
C
	      ELSE
C
C*		When data is above the surface, find the first and last
C*		point to plot.
C
		knt = 0
		i1  = 0
		i2  = 0
		i   = 1
		done= .false.
		DO WHILE ( (.not. done) .and. (i .le. LLMAXD) )
		    IF  ( .not. ERMISS (pontha (i,lev))) THEN
			knt = knt + 1
			xx (knt+1) = x (i)
			yy (knt+1) = pontha (i,lev)
			IF  ( i1 .eq. 0 ) i1 = i
			i2 = i
		      ELSE IF ( knt .gt. 0 ) THEN
			done = .true.
		    END IF
		    i = i + 1
		END DO
C
C*		Check if there are points to plot.
C
		IF  ( knt .gt. 2 ) THEN
		    istart = 2
		    knt = knt + 1
C
C*		    If data does not cover entire grid, make sure
C*		    isentrope is extended to closest reporting station.
C
		    IF  ( i1 .ne. 1 ) THEN
			CALL SNSSUR ( lev, i1, nstn, sloc, x, pdat,
     +                                 istn1,
     +					istn2, ier )
			IF ( istn1 .ne. 0 ) THEN
			    xx (1) = sloc (istn1)
			    yy (1) = pdat (istn1,lev)
			    istart = 1
			END IF
		    END IF
		    IF  ( i2 .ne. LLMAXD ) THEN
			CALL SNSSUR ( lev, i2, nstn, sloc, x, pdat,
     +                                  istn1,
     +					istn2, ier )
			IF ( istn2 .ne. 0 ) THEN
			    knt = knt + 1
			    xx (knt) = sloc (istn2)
			    yy (knt) = pdat (istn2, lev)
			END IF
		    END IF
		    IF ( istart .eq. 2 ) knt = knt - 1
		    CALL GLINE ( 'M', knt, xx(istart), yy(istart), ier )
		END IF
	    END IF
C
C*	    Label isentrope if at end of line and labelling requested.
C*	    The labels are only written when the isentrope intersects
C*	    the right-hand edge.
C
	    IF  (( .not. ERMISS (pontha (LLMAXD,lev)) ) .and. 
     +           ( isnlbl (ilc) .ne. 0 )            .and.
     +           ( pontha (LLMAXD, lev) .le. ybot )     .and.
     +           ( pontha (LLMAXD, lev) .ge. ytop ))  THEN
		IF  ( isnlbl (ilc) .eq. 1 ) THEN
		    icurth = curtha
		    CALL ST_INCH ( icurth, label, ier )
		    CALL GTRANS ( 'M', 'N', 1, sloc (nstn), 
     +				  pontha (LLMAXD,lev), xlbl, ylbl, ier )
		    CALL GTEXT  ( 'N', xlbl, ylbl, label, 0., 2, 0,
     +				  ier )
		END IF
	    END IF
C
C*	    Increment counters.
C
	    curtha = curtha + thinc
	    lev    = lev  + 1
	    ilc    = ilc  + 1
C*
	  END DO
C
C*	  Reset line type.
C
	  CALL GSLINE  ( il1, 0, il3, 0, ier )
	END IF
C*
	RETURN
	END
