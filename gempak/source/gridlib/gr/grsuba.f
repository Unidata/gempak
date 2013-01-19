	SUBROUTINE GR_SUBA ( garea, fill, rnvblk, altln, ksubx, ksuby,
     +			    subset, iret )
C************************************************************************
C* GR_SUBA								*
C*									*
C* This subroutine determines the navigation of a grid subset.  The	*
C* input navigation block is used to set a navigation which is then	*
C* subset using the input in GAREA.  GAREA is either @ followed by	*
C* actual grid index coordinates of the lower-left and upper right	*
C* points of the subset rectangle, or it is the lat/lon coordinates	*
C* of the two corner points.  In either case, the four numbers are	*
C* separated by semicolons.  In the latter case, the nearest grid	*
C* points are used.							*
C*									*
C* Once the subset is determined the navigation block is modified	*
C* accordingly.  The subset coordinates relative to the larger grid	*
C* are set and returned in KSUBX and KSUBY.  The lat/lon coordinates	*
C* of the corners are returned in ALTLN.				*
C*									*
C* If the grid is an eta staggered grid, then the area is adjusted so   *
C* that the corner points are h points.					*
C*									*
C* GR_SUBA ( GAREA, FILL, RNVBLK, ALTLN, KSUBX, KSUBY, SUBSET, IRET )	*
C*									*
C* Input parameters:							*
C*	GAREA		CHAR*		Input for subset area		*
C*									*
C* Input and output parameters:						*
C*	FILL		LOGICAL		Flag for filled staggered grid  *
C*	RNVBLK (*)	REAL		Grid navigation block		*
C*									*
C* Output parameters:							*
C*	ALTLN (4)	REAL		Lat/lon bounds of subset	*
C*	KSUBX (2)	INTEGER		Start/stop x index of subset	*
C*	KSUBY (2)	INTEGER		Start/stop y index of subset	*
C*	SUBSET		LOGICAL		Subset flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal			*
C*					 -4 = Invalid navigation info	*
C*					-20 = Invalid input for GAREA	*
C*									*
C**									*
C* Log:									*
C* K. Brill/NMC		 4/95						*
C* S. Jacobs/NMC	 5/95	Added call to LC_GARE to parse GAREA	*
C* L. Sager/NMC		 8/95	Added NAGSS2 to permit grid wrapping	*
C* K. Brill/EMC		 4/96	Added fill to start on h point		*
C* G. Krueger/EAI	 6/96	Add default projection			*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* D.W.Plummer/NCEP	 3/00	Re-name from NAGSUB to GR_SUBA and	*
C* 				Re-name from NAGSS2 to GR_SUB2		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	garea
	REAL		rnvblk (*), altln (4)
	INTEGER		ksubx (2), ksuby (2)
	LOGICAL		fill, subset
C*
	LOGICAL		wrap
C*
	CHARACTER	sare*72, cdproj*30
	REAL		ssrg (4), centrd (2)
	IHADJM ( i, j, ix ) =
     +		     ( i - IABS ( MOD ( ( (j-1)*ix + i ), 2 ) - 1 ) )
	IHADJP ( i, j, ix ) =
     +		     ( i + IABS ( MOD ( ( (j-1)*ix + i ), 2 ) - 1 ) )
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for subsetting
C
	CALL ST_LCUC ( garea, sare, ier )
	IF  ( ( sare .eq. ' '    ) .or.
     +	      ( sare .eq. 'GRID' ) .or.
     +	      ( sare .eq. 'DSET' ) )  THEN
	    subset = .false.
	    RETURN
	END IF
C
C*	Set the full-grid navigation.
C
	subset = .true.
	CALL GR_SNAV ( LLNNAV, rnvblk, iret )
	IF ( iret .ne. 0 ) THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Compute a subset navigation.
C
C*	If an @ precedes the numbers, grid point coordinates are
C*	being input; otherwise, process the input as normal for GAREA.
C
C*	Turn everything into grid point values.
C
	IF  ( sare(1:1) .eq. '@' )  THEN
	    sare (1:1) = ' '
	    CALL ST_RLST ( sare, ';', RMISSD, 4, ssrg, n, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -20
		RETURN
	    END IF
	    rgx1 = FLOAT ( NINT ( ssrg (1) ) )
	    rgy1 = FLOAT ( NINT ( ssrg (2) ) )
	    rgx2 = FLOAT ( NINT ( ssrg (3) ) )
	    rgy2 = FLOAT ( NINT ( ssrg (4) ) )
	ELSE
	    CALL LC_GARE ( sare, ssrg, cdproj, centrd, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -20
		RETURN
	    END IF
C
C*	    Turn the area into whole grid point locations.
C
	    CALL GTRANS ( 'M', 'G', 1, ssrg (1), ssrg (2),
     +			  rgx1, rgy1, ier )
	    rgx1 = FLOAT ( NINT ( rgx1 ) )
	    rgy1 = FLOAT ( NINT ( rgy1 ) )
	    CALL GTRANS ( 'M', 'G', 1, ssrg (3), ssrg (4),
     +			  rgx2, rgy2, ier )
	    rgx2 = FLOAT ( NINT ( rgx2 ) )
	    rgy2 = FLOAT ( NINT ( rgy2 ) )
	END IF
	IF ( fill ) THEN
	    ix = NINT ( rnvblk (5) )
	    i1 = NINT ( rgx1 )
	    j1 = NINT ( rgy1 )
	    i2 = NINT ( rgx2 )
	    j2 = NINT ( rgy2 )
	    i1 = IHADJM (i1,j1,ix)
	    i2 = IHADJP (i2,j2,ix)
	    rgx1 = i1
	    rgy1 = j1
	    rgx2 = i2
	    rgy2 = j2
	END IF
C*
	wrap = .false.
	IF ( rgx2 .le. rgx1 ) THEN
	    CALL GR_SUB2 ( rnvblk, rgx2, igxold, ierr )
	    IF ( ierr .eq. 0 ) wrap = .true.
	END IF
	IF (  rgy2 .le. rgy1 .or.
     +	     ier .ne. 0 ) THEN
	    iret = -20
	    RETURN
	END IF
C
C*	Check to see if subset area is a proper subset of the
C*	input grid.
C
	IF ( .not. wrap ) THEN
		IF ( rgx1 .lt. 1. .or. rgx1 .gt. rnvblk (5) .or.
     +       	rgx2 .lt. 1. .or. rgx2 .gt. rnvblk (5) .or.
     +	    	rgy1 .lt. 1. .or. rgy1 .gt. rnvblk (6) .or.
     +	    	rgy2 .lt. 1. .or. rgy2 .gt. rnvblk (6) ) THEN
	    	iret = -20
	    	RETURN
		END IF
	END IF
C
C*	Now turn the rounded-off grid point coordinates into
C*	lat/lon.
C
	CALL GTRANS ( 'G', 'M', 1, rgx1, rgy1, ssrg (1),
     +		      ssrg (2), ier )
	CALL GTRANS ( 'G', 'M', 1, rgx2, rgy2, ssrg (3),
     +		      ssrg (4), ier )
C
C*	Reset expected navigation corner points.
C
	issll = rgx1
	jssll = rgy1
	issur = rgx2
	jssur = rgy2
	rnvblk (5) = issur - issll + 1
	rnvblk (6) = jssur - jssll + 1
	ii = 0
	DO inv = 7, 10
	    ii = ii + 1
	    rnvblk (inv) = ssrg (ii)
	END DO
	ksubx (1) = issll
	ksuby (1) = jssll
	ksubx (2) = issur
	ksuby (2) = jssur
	DO i = 1, 4
	    altln (i) = ssrg (i)
	END DO
C*
	RETURN
	END
