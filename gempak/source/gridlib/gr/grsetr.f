	SUBROUTINE GR_SETR  ( kxin, kyin, ishft, iret )
C************************************************************************
C* GR_SETR								*
C*									*
C* This subroutine sets the shift position needed to rearrange a globe	*
C* wrapping grid on either a CED or MER projection.  The rearrangement	*
C* must be done for any display map projection when the grid discon-	*
C* tinuity occurs within the map area.  The rearrangement creates a	*
C* continuous grid of data over the map	area.				*
C* 									*
C* Warning:  This subroutine resets the grid navigation in GPLT to be	*
C*           that of the re-arranged grid.				*
C* 									*
C* GR_SETR  ( KXIN, KYIN, ISHFT, IRET )					*
C*									*
C* Input parameters:							*
C*	KXIN		INTEGER		Number of input points in x dir	*
C*	KYIN		INTEGER		Number of input points in y dir	*
C*									*
C* Output parameters:							*
C*	ISHFT		INTEGER		X index shift number needed by	*
C*					subroutine GR_DORG		*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-21 = cannot fix wrap around	*
C*					-22 = no map projection is set	*
C**									*
C* Log:									*
C* K. Brill/HPC		08/02	Created from original GR_RARG code with	*
C*				added longitude buffer zone and	check	*
C*				at +/-5 grid pts from grd discontinuity *
C* K. Brill/HPC		09/02	Do only check at columns KX-5, KX, and 5*
C* S. Jacobs/NCEP	10/02	Removed check for +/-5 grid pt columns	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rglt (2), rgln (2), rgx (2), rgy (2)
	CHARACTER*4	mprj, gprj
	LOGICAL		compar, vis
C*
	compar (xx, yy) = ( ABS ( xx - yy ) .lt. RDIFFD )
C------------------------------------------------------------------------
	iret = 0
	ishft = 0
C
C*	Find map and grid navigation.
C
	CALL GQMPRJ ( mprj, am1, am2, am3, amlt1, amln1,
     +		      amlt2, amln2, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
	    iret = -22
	    RETURN
	END IF
C*
	CALL GQGPRJ ( gprj, ag1, ag2, ag3, mx, my, aglt1, agln1,
     +		      aglt2, agln2, ier ) 
C
C*	Return if angles 1 and 3 are not zero.
C
	IF  ( ag1 .ne. 0. .or. ag3 .ne. 0. ) RETURN
C
	IF ( ier .ne. 0 .or. ( kxin .ne. mx .or.
     +			       kyin .ne. my ) ) THEN
	    CALL ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
	    iret = -21
	    RETURN
	END IF
C
C*	If it is not a CED or MER grid, i.e., not a global
C*	grid, no shifting is necessary.
C
	IF ( gprj .ne. 'MER' .and. gprj .ne. 'CED' ) RETURN
C
C*	Return for non-global grid.
C
	rgx ( 1 ) = 1.
	rgy ( 1 ) = 1.
	rgx ( 2 ) = float ( mx )
	rgy ( 2 ) = 1.
	CALL GTRANS ( 'G', 'M', 2, rgx, rgy, rglt, rgln, ier )
	IF ( .not. ( compar ( rgln (1), rgln (2) ) ) .and.
     +  .not. ( compar ( (rgln (1) + 360.), rgln (2) ) ) ) RETURN
C
C*	Check column at KX for visibility.
C
	vis = .false.
	iy = 0
	DO WHILE ( iy .lt. kyin .and. .not. vis )
	    iy = iy + 1
	    rglt (1) = FLOAT ( kxin )
	    rgln (1) = FLOAT ( iy )
	    CALL GPTVIS ( 'G', 1, rglt, rgln, vis, ier )
	END DO
C
C*	If the column of discontinuity is found on the map,
C*	rearrange the grid.
C
	IF  ( vis )  THEN
C
C*	    Eliminate the duplicate grid column so that the first
C*	    (last) column will not be shifted twice.
C
	    kxm1 = kxin - 1
C
	    IF  ( mprj .eq. 'CED' .or. mprj .eq. 'MER' )  THEN
C
C*	        Find the range of grid indexes covering the map.
C
	        rglt (1) = amlt1
	        rgln (1) = amln1
	        rglt (2) = amlt2
	        rgln (2) = amln2
	        CALL GTRANS ( 'M', 'G', 2, rglt, rgln, rgx, rgy,
     +	    		      ier )
C
C*	        Check for no need to rarrange.
C
	        IF ( compar ( rgx (1), 1. ) .or.
     +		     compar ( rgx (2), 1. ) )  RETURN
C
C*	        Find range of grid indexes covered by map.
C
	        dif = rgx (2) - rgx (1)
	        IF ( dif .le. 0 ) dif = dif + kxm1
C
C*	        Compute x grid index desired at left of map.
C
	        ileft = NINT ( ( kxm1 - dif ) / 2. + .5 )
		ishft = NINT ( rgx (1) ) - ileft
	      ELSE
C
C*		For non CED/MER map projection, rearrange the grid
C*		in such a way that the grid discountinuity occurs
C*		on the opposite side of the central longitude.
C
		clon = am2 - 180.
		IF  ( clon .le. -180. ) clon = clon + 360.
		rglt (1) = 0.
		rgln (1) = clon
	        CALL GTRANS ( 'M', 'G', 1, rglt, rgln, rgx, rgy, ier )
		ishft = NINT ( rgx (1) ) - 1.
	    END IF
C
C*	    Reset the grid navigation.
C
	    rgx (1) = ishft
	    rgy (1) = 1.
	    CALL GTRANS ( 'G', 'M', 1, rgx, rgy, rglt, rgln, ier )
	    CALL GSGPRJ ( gprj, 0., 0., 0., kxin, kyin, aglt1,
     +			  rgln (1), aglt2, rgln (1), ier )
	    IF ( ier .ne. 0 ) THEN
		CALL ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
		iret = -21
	    END IF
	END IF
C*
	RETURN
	END
