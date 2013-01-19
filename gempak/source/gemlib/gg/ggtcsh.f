	SUBROUTINE GG_TCSH ( xlat1, xlon1, np1, xlat2, xlon2, np2, 
     +			     ylat, ylon, npts, iret )
C************************************************************************
C* GG_TCSH								*
C*									*
C* This subroutine calculates the bound of two intersecting polygons.   *
C* It works with polygons which are defined by many closely-spaced      *
C* points, e.g., circular shapes.                                       *
C*                                                                      *
C* GG_TCSH ( XLAT1, XLON1, NP1, XLAT2, XLON2, NP2, YLAT, YLON, NPTS,    *
C*	     IRET )                                                     *
C*									*
C* Input parameters:							*
C*	XLAT1 (*)	REAL		Latitudes of first polygon      *
C*	XLON1 (*)	REAL		Longitudes of first polygon     *
C*	NP1		INTEGER		Number of points in first poly  *
C*	XLAT2 (*)	REAL		Latitudes of second polygon     *
C*	XLON2 (*)	REAL		Longitudes of second polygon    *
C*	NP2		INTEGER		Number of points in second poly *
C*									*
C* Output parameters:							*
C*	YLAT (*)	REAL		Latitudes of bound              *
C*	YLON (*)	REAL		Longitudes of bound             *
C*	NPTS		INTEGER		Number of points in bound       *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					 -1 = polygons do not intersect *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/01	                                        *
C* D. Kidwell/NCEP	10/01	Use clo_dist; check nseg1 .eq. nseg2    *
C* D. Kidwell/NCEP	 2/02	Removed parameter idrop from call seq.  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MXPTS = LLMXDT / 2, MXPTS4 = MXPTS + 400 )
C*
	REAL		xlat1 (*), xlon1 (*), xlat2 (*), xlon2 (*),
     +			ylat (*), ylon (*)
C*
	CHARACTER	sys*2
	INTEGER		ino12 (MXPTS4), ino21 (364), ipt1 (2,100), 
     +			ipt2 (2,100), indx1 (100), indx2 (100)
	LOGICAL		done, okay, use1 (100), use2 (100)
C-----------------------------------------------------------------------
	iret = 0
	npts = 0
	okay = .false.
C
C*	First check for no points.
C
	IF ( ( np1 .eq. 0 ) .or. ( np2 .eq. 0 ) ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Find the bounding segments.
C
	sys = 'M'
	CALL ST_NULL ( sys, sys, lens, ier )
	CALL CGR_INPOLY ( sys, np1, xlat1, xlon1, sys, np2, xlat2, 
     +		          xlon2, ino12, ier )
	CALL GG_TCEP ( xlat1, xlon1, np1, ino12, ipt1, nseg1, ier )
	IF ( nseg1 .eq. 0 ) THEN
	    okay = .true.
	  ELSE
	    CALL CGR_INPOLY ( sys, np2, xlat2, xlon2, sys, np1, xlat1, 
     +			      xlon1, ino21, ier )
	    CALL GG_TCEP ( xlat2, xlon2, np2, ino21, ipt2, nseg2, ier )
	    IF ( nseg2 .eq. 0 ) THEN
		okay = .true.
	    END IF
	END IF
C
C*	Check for two single segments.
C
	IF ( ( nseg1 .eq. 1 ) .and. ( nseg2 .eq. 1 ) ) THEN
	    okay = .true.
	END IF
C
C*	Check whether the number of segments may need to be adjusted.
C
	IF ( .not. okay ) THEN
C
C*	    Eliminate the shortest segments from the first polygon.
C
	    done = .false.
	    ii   = 1
	    DO WHILE ( .not. done )
		len = ipt1 ( 2, ii ) - ipt1 ( 1, ii )
		IF ( len .lt. 0 ) len = len + np1
 		IF ( len .lt. 5 ) THEN
		    nseg1 = nseg1 - 1
		    DO jj = ii, nseg1
			ipt1 ( 1, jj ) = ipt1 ( 1, jj + 1 )
			ipt1 ( 2, jj ) = ipt1 ( 2, jj + 1 )
		    END DO
		  ELSE
		    ii = ii + 1
		END IF
		IF ( ii .gt. nseg1 ) done = .true.
	    END DO    
C
C*	    Eliminate the shortest segments from the second polygon.
C
	    done = .false.
	    ii   = 1
	    DO WHILE ( .not. done )
		len = ipt2 ( 2, ii ) - ipt2 ( 1, ii )
		IF ( len .lt. 0 ) len = len + np2
 		IF ( len .lt. 5 ) THEN
		    nseg2 = nseg2 - 1
		    DO jj = ii, nseg2
			ipt2 ( 1, jj ) = ipt2 ( 1, jj + 1 )
			ipt2 ( 2, jj ) = ipt2 ( 2, jj + 1 )
		    END DO
		  ELSE
		    ii = ii + 1
		END IF
		IF ( ii .gt. nseg2 ) done = .true.
	    END DO    
C
	    IF ( ( nseg1 .gt. nseg2 ) .and. ( nseg2 .gt. 0 ) ) THEN
C
C*		Combine segments in the first polygon until it has the
C*		same number of segments as the second.
C
		DO WHILE ( nseg1 .gt. nseg2 )
		    mingap = np1 + 1
		    DO ii = 1, nseg1
		        init = ii + 1
		        IF ( init .gt. nseg1 ) init = 1
		        igap = ipt1 ( 1, init ) - ipt1 ( 2, ii ) 
		        IF ( igap .lt. 0 ) igap = np1 + igap
		        IF ( igap .lt. mingap ) THEN
			    mingap = igap
			    iskip  = ii
		        END IF
		    END DO
C
		    IF ( iskip .lt. nseg1 ) THEN
		        ipt1 ( 2, iskip ) = ipt1 ( 2, iskip + 1 )
		      ELSE
		        ipt1 ( 2, iskip ) = ipt1 ( 2, 1 )
		        iskip = 0
		    END IF
		    nseg1 = nseg1 - 1
		    DO ii = iskip + 1, nseg1
		        ipt1 ( 1, ii ) = ipt1 ( 1, ii + 1 )
		        ipt1 ( 2, ii ) = ipt1 ( 2, ii + 1 ) 
		    END DO
		END DO
C		
	      ELSE IF ( ( nseg2 .gt. nseg1 ) .and. ( nseg1 .gt. 0 ) ) 
     +			  THEN
C
C*		Combine segments in the second polygon until it has the
C*		same number of segments as the first.
C
		DO WHILE ( nseg2 .gt. nseg1 )
		    mingap = np2 + 1
		    DO ii = 1, nseg2
		        init = ii + 1
		        IF ( init .gt. nseg2 ) init = 1
		        igap = ipt2 ( 1, init ) - ipt2 ( 2, ii ) 
		        IF ( igap .lt. 0 ) igap = np2 + igap
		        IF ( igap .lt. mingap ) THEN
			    mingap = igap
			    iskip  = ii
		        END IF
		    END DO
C
		    IF ( iskip .lt. nseg2 ) THEN
		        ipt2 ( 2, iskip ) = ipt2 ( 2, iskip + 1 )
		      ELSE
		        ipt2 ( 2, iskip ) = ipt2 ( 2, 1 )
		        iskip = 0
		    END IF
		    nseg2 = nseg2 - 1
		    DO ii = iskip + 1, nseg2
		        ipt2 ( 1, ii ) = ipt2 ( 1, ii + 1 )
		        ipt2 ( 2, ii ) = ipt2 ( 2, ii + 1 ) 
		    END DO
		END DO
	    END IF
	END IF
C
C*	Look for special cases.
C
	IF ( nseg1 .le. 0 ) THEN
	    npts = np2
	    DO ii = 1, npts
		ylat ( ii ) = xlat2 ( ii )
		ylon ( ii ) = xlon2 ( ii ) 
	    END DO
	    RETURN
	  ELSE IF ( nseg2 .le. 0 ) THEN
	    npts = np1
	    DO ii = 1, npts
		ylat ( ii ) = xlat1 ( ii )
		ylon ( ii ) = xlon1 ( ii ) 
	    END DO
	    RETURN
	  ELSE IF ( ( nseg1 .eq. 1 ) .and. ( nseg2 .eq. 1 ) ) THEN
	    IF ( ( ipt1 (1,1) .eq. 1 ) .and. 
     +		 ( ipt1 (2,1) .eq. np1 ) .and. 
     +	         ( ipt2 (1,1) .eq. 1 ) .and.
     +		 ( ipt2 (2,1) .eq. np2 ) ) THEN
	        iret = -1
	        RETURN
	    END IF
	END IF
C
C*	There are now an equal number of segments in the two polygons,
C*	so that nseg1 = nseg2.  Find the longest segment in the first 
C*	polygon and get its endpoints.
C
	DO ii = 1, nseg1
	    use1 ( ii ) =.false.
	    use2 ( ii ) = .false.
	END DO
C
	length = 0
	iend   = ipt1 ( 2, 1 )
	ibeg1  = 1
	DO ii = 1, nseg1
	    len = ipt1 ( 2, ii ) - ipt1 ( 1, ii ) + 1
	    IF ( len .lt. 0 ) len = np1 + len
	    IF ( len .gt. length ) THEN
		length = len
		iend   = ipt1 ( 2, ii )
		ibeg1  = ii
	    END IF
	END DO
	indx1 ( 1 ) = ibeg1
	use1 ( ibeg1 ) = .true.
C
C*	Match the closest endpoints of the two polygons.
C
	done = .false.
	iseg = 1
	DO WHILE ( .not. done )
	    x1    = xlat1 ( iend )
	    y1    = xlon1 ( iend )
	    dd    = 1000000.
	    ibeg2 = 1
	    DO ii = 1, nseg1
		IF ( .not. use2 ( ii ) ) THEN
	            x2 = xlat2 ( ipt2 ( 1, ii ) )
	            y2 = xlon2 ( ipt2 ( 1, ii ) )
	            CALL CLO_DIST ( x1, y1, 1, x2, y2, dst, ier )
	            IF ( dst .lt. dd ) THEN
		        dd    = dst
		        ibeg2 = ii
	            END IF	
		END IF
	    END DO
C
	    indx2 ( iseg ) = ibeg2
	    use2 ( ibeg2 ) = .true.
	    iend = ipt2 ( 2, ibeg2 )
	    iseg = iseg + 1
	    IF ( iseg .le. nseg1 ) THEN 
	        x1    = xlat2 ( iend )
	        y1    = xlon2 ( iend )
	        dd    = 1000000.
	        ibeg1 = 1
	        DO ii = 1, nseg1
		    IF ( .not. use1 ( ii ) ) THEN
	                x2 = xlat1 ( ipt1 ( 1, ii ) )
	                y2 = xlon1 ( ipt1 ( 1, ii ) )
	                CALL CLO_DIST ( x1, y1, 1, x2, y2, dst, ier )
	                IF ( dst .lt. dd ) THEN
		            dd    = dst
		            ibeg1 = ii
	                END IF	
		    END IF
	        END DO
C
	        indx1 ( iseg ) = ibeg1
	        use1 ( ibeg1 ) = .true.
		iend = ipt1 ( 2, ibeg1 )
	      ELSE
		done = .true.
	    END IF
	END DO
C
C*	Get the bounds for the overlapping polygons.
C
	done   = .false.
	ipoint = 1
	iseg   = 1
C
	DO WHILE ( .not. done )
	    jj   = ipt1 ( 1, indx1 ( iseg ) )
	    jend = ipt1 ( 2, indx1 ( iseg ) )
	    jseg = jend - jj + 1
	    IF ( jseg .le. 0 ) jseg = np1 + jseg
	    IF ( jj .gt. np1 ) jj = jj - np1
	    istop = ipoint + jseg - 1
	    DO ii = ipoint, istop
		ylat ( ii ) = xlat1 ( jj ) 
		ylon ( ii ) = xlon1 ( jj ) 
		jj = jj + 1
		IF ( jj .gt. np1 ) jj = 1
	    END DO
	    ipoint = istop + 1
C
	    jj   = ipt2 ( 1, indx2 ( iseg ) )
	    jend = ipt2 ( 2, indx2 ( iseg ) )
	    jseg = jend - jj + 1
	    IF ( jseg .le. 0 ) jseg = np2 + jseg
	    IF ( jj .gt. np2 ) jj = jj - np2
	    istop = ipoint + jseg - 1
	    DO ii = ipoint, istop
	        ylat ( ii ) = xlat2 ( jj ) 
	        ylon ( ii ) = xlon2 ( jj ) 
	        jj = jj + 1
	        IF ( jj .gt. np2 ) jj = 1
	    END DO
	    ipoint = istop + 1
	    iseg   = iseg + 1
	    IF ( iseg .gt. nseg1 ) done = .true.
	END DO
C
	npts = ipoint
	ylat ( npts ) = ylat ( 1 )
	ylon ( npts ) = ylon ( 1 )
C*
	RETURN
	END
