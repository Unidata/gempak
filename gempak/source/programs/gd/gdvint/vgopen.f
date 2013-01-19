	SUBROUTINE VG_OPEN  ( iret )
C************************************************************************
C* VG_OPEN								*
C*									*
C* This subroutine opens the grid files for the vertical interpolation  *
C* program.  GEMPAK files are used.					*
C*									*
C* VG_OPEN ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C*					-11 = invalid subset region	*
C*					-12 = file open failed		*
C*					-15 = grid size is too large	*
C**									*
C* Log:									*
C* K. Brill/NMC      	06/92						*
C* K. Brill/NMC		08/92	Added subsetting			*
C* K. Brill/NMC		02/93	Check return code from TG_FULL		*
C* K. Brill/NMC		03/93	Get subset from existing output file	*
C* K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* K. Brill/HPC		11/02	Check grid size against LLMXGD		*
C* R. Tian/SAIC		 4/05	GR_OPEN -> GD_OPEN			*
C* G. Hull/SAIC	         3/08   rm LLMXGD
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL		rnav (LNAV), rnv2 (LNAV), rnavsb (LNAV),
     +			ranl ( LANL ), ssrg (4)
	CHARACTER*20	times (LLMXGT)
	CHARACTER*32	sare
	LOGICAL		exist, gpts
C-----------------------------------------------------------------------
	iret = 0
	wrtflg = .false.
C
C*	Open the input file.
C
	CALL FL_INQR ( gdfile, exist, gdcuri, iret )
	IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	IF ( gdfile .eq. gdoutf ) wrtflg = .true.
	CALL GD_OPEN ( gdcuri, wrtflg, LANL, LNAV, igdfli, ranl,
     +		       rnav, mxgrd, iret )
	IF ( iret .ne. 0 ) THEN
    	    CALL ER_WMSG ( 'GD', iret, gdfile, ier )
	    iret = -12
	    RETURN
	END IF
	insz = 13
	iasz = 128
	CALL GR_SNAV ( insz, rnav, ier )
C
C*	Set the expected navigation block.  This is the navigation
C*	block the output grid should have.  It is initialized to
C*	the input navigation.
C
	DO inv = 1, insz
	    rnavsb (inv) = rnav (inv)
	END DO
	issll = 1
	jssll = 1
	issur = rnav (5)
	jssur = rnav (6)
	kxin = issur
	kyin = jssur
	kxy = kxin * kyin
C*
	IF ( .not. wrtflg ) THEN
C
C*	    Check to see if the output file exists.
C
	    CALL FL_INQR ( gdoutf, exist, gdcuro, ire )
C*
	    IF ( exist ) THEN
C*
	    	CALL GD_OPEN ( gdcuro, .true., LANL, LNAV, igdflo,
     +			       ranl, rnv2, mxgrd, iret )
	    	IF ( iret .ne. 0 ) THEN
	    	    CALL ER_WMSG ( 'GD', iret, gdoutf, ier )
	            iret = -12
		    RETURN
	    	END IF
C
C*	    	Ignore the input subset area and use the navigation
C*		in this file to create the subset.
C
		DO i = 1, 13
		    rnavsb (i) = rnv2 (i)
		END DO
		ii = 0
	    	DO inv = 7, 10
		    ii = ii + 1
		    ssrg (ii) = rnv2 (inv)
	    	END DO
C
C*		Turn lat/lon of corner points into grid indexes
C*		on the input grid.  Then check to see if these
C*		indexes give the same grid dimensions as specified
C*		in the output file navigation.
C
		CALL GTRANS ( 'M', 'G', 1, ssrg (1), ssrg (2),
     +			          rgx1, rgy1, ier )
		rgx1 = FLOAT ( NINT ( rgx1 ) )
		rgy1 = FLOAT ( NINT ( rgy1 ) )
		CALL GTRANS ( 'M', 'G', 1, ssrg (3), ssrg (4),
     +		              rgx2, rgy2, ier )
		rgx2 = FLOAT ( NINT ( rgx2 ) )
		rgy2 = FLOAT ( NINT ( rgy2 ) )
	    	IF ( rgx1 .lt. 1. .or. rgx1 .gt. rnav (5) .or.
     +		     rgx2 .lt. 1. .or. rgx2 .gt. rnav (5) .or.
     +		     rgy1 .lt. 1. .or. rgy1 .gt. rnav (6) .or.
     +		     rgy2 .lt. 1. .or. rgy2 .gt. rnav (6) ) THEN
		    iret = -15
		    RETURN
	    	END IF
C
C*		Set the subset region.
C
	    	issll = rgx1
	    	jssll = rgy1
	    	issur = rgx2
	    	jssur = rgy2
	    	knav5 = issur - issll + 1
	    	knav6 = jssur - jssll + 1
C
C*		Check that projection, angles and grid dimensions
C*		are correct and consistent.
C
		IF ( knav5 .ne. INT ( rnv2 (5) ) .or.
     +		     knav6 .ne. INT ( rnv2 (6) ) .or.
     +		     rnv2  (2) .ne. rnav  (2) .or.
     +		     rnv2 (11) .ne. rnav (11) .or.
     +		     rnv2 (12) .ne. rnav (12) .or.
     +		     rnv2 (13) .ne. rnav (13) ) THEN
		    CALL VG_IGEN ( gdoutf, rnv2, 1, 6, ier ) 
		    iret = -15
		    RETURN
	    	END IF
C*
	    ELSE
C
C*		Create a new file.
C
C*	        Compute a subset navigation if necessary.
C
	    	CALL ST_LCUC ( area, sare, ier )
	    	IF ( sare .ne. ' ' .and. sare .ne. 'GRID' ) THEN
C
C*	            Split input into 4 real numbers.  If an @ precedes
C*	    	    the numbers, grid point coordinates are being input;
C*	    	    otherwise, lat/lon values are entered.
C
	    	    IF ( sare(1:1) .eq. '@' ) THEN
		    	gpts = .true.
		    	sare (1:1) = ' '
	    	    ELSE
		    	gpts = .false.
	    	    END IF
	    	    CALL ST_RLST ( sare, ';', RMISSD, 4, ssrg, n, ier )
	    	    IF ( ier .ne. 0 ) THEN
		    	iret = -11
		    	RETURN
	    	    END IF
C
C*	    	    Turn everything into grid point values.
C
	    	    IF ( gpts ) THEN
		    	rgx1 = FLOAT ( NINT ( ssrg (1) ) )
		    	rgy1 = FLOAT ( NINT ( ssrg (2) ) )
		    	rgx2 = FLOAT ( NINT ( ssrg (3) ) )
		    	rgy2 = FLOAT ( NINT ( ssrg (4) ) )
	    	    ELSE
C
C*		    	Turn lat/lon into whole grid point locations.
C
		    	CALL GTRANS ( 'M', 'G', 1, ssrg (1), ssrg (2),
     +			              rgx1, rgy1, ier )
		    	rgx1 = FLOAT ( NINT ( rgx1 ) )
		    	rgy1 = FLOAT ( NINT ( rgy1 ) )
		    	CALL GTRANS ( 'M', 'G', 1, ssrg (3), ssrg (4),
     +			              rgx2, rgy2, ier )
		    	rgx2 = FLOAT ( NINT ( rgx2 ) )
		    	rgy2 = FLOAT ( NINT ( rgy2 ) )
	    	    END IF
	    	    IF ( rgx2 .le. rgx1 .or. rgy2 .le. rgy1 .or.
     +		         ier .ne. 0 ) THEN
		    	iret = -11
		    	RETURN
	    	    END IF
C
C*	    	    Check to see if subset area is a proper subset of the
C*	    	    input grid.
C
	    	    IF ( rgx1 .lt. 1. .or. rgx1 .gt. rnav (5) .or.
     +		         rgx2 .lt. 1. .or. rgx2 .gt. rnav (5) .or.
     +		         rgy1 .lt. 1. .or. rgy1 .gt. rnav (6) .or.
     +		         rgy2 .lt. 1. .or. rgy2 .gt. rnav (6) ) THEN
		    	WRITE (6,*) ' Input grid dimensions are ',
     +			          rnav(5), ' by ', rnav(6)
		    	iret = -11
		    	RETURN
	    	    END IF
C
C*	    	    Now turn the rounded-off grid point coordinates into
C*	    	    lat/lon.
C
	    	    CALL GTRANS ( 'G', 'M', 1, rgx1, rgy1, ssrg (1),
     +			          ssrg (2), ier )
	    	    CALL GTRANS ( 'G', 'M', 1, rgx2, rgy2, ssrg (3),
     +			  	  ssrg (4), ier )
C
C*	    	    Reset expected navigation corner points.
C
	    	    issll = rgx1
	    	    jssll = rgy1
	    	    issur = rgx2
	    	    jssur = rgy2
	    	    rnavsb (5) = issur - issll + 1
	    	    rnavsb (6) = jssur - jssll + 1
	    	    ii = 0
	    	    DO inv = 7, 10
		    	ii = ii + 1
		    	rnavsb (inv) = ssrg (ii)
	    	    END DO
	        END IF
C
C*	    	Create a new grid file for the output.
C
	    	CALL ST_NUMB ( maxgrd, imxgrd, ier )
		ihdsz = 8
	    	CALL GD_CREF ( gdoutf, insz, rnavsb, iasz, ranl,
     +			       ihdsz, imxgrd, igdflo, iret )
	    	IF ( iret .ne. 0 ) THEN
	    	    CALL ER_WMSG ( 'GD', iret, gdoutf, ier )
	            iret = -12
		    RETURN
	    	END IF
		gdcuro = gdoutf
	    END IF
	ELSE
C
C*	    Assign the input file name to the output file name.
C
	    gdcuro = gdcuri
	END IF
C
C*	Set grid size.
C
	kx = rnavsb (5)
	ky = rnavsb (6)
	kxky = kx * ky
C
C*	Set the grid time.
C
	CALL GD_GTIM ( igdfli, LLMXGT, times, last, ier ) 
	CALL TG_FULL ( gdattm, times(1), times (last), gdttm (1),
     +		       iret )
	IF ( iret .ne. 0 ) THEN
	    CALL ER_WMSG ( 'TG', iret, gdattm, ier )
	    iret = -8
	END IF
C*
	gdttm (2) = ' '
C*
	RETURN
	END
