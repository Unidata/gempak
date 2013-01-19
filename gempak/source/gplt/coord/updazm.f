	SUBROUTINE UPDAZM  ( maptyp, iret )
C************************************************************************
C* UPDAZM								*
C*									*
C* This subroutine updates the map common area when map azimuthal	*
C* coordinates are defined.  This subroutine may be used to set up	*
C* either the map or grid linear coordinate system.  For the map	*
C* coordinate setup, maptyp must be true; for grid coordinates,		*
C* maptyp must be false.						*
C*									*
C* UPDAZM  ( MAPTYP, IRET )						*
C*									*
C* Input parameters:							*
C*	MAPTYP		LOGICAL		Map type			*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC							*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	 4/89	Eliminated use of angle3		*
C* K. Brill/GSC          1/90   Check for dateline crossing		*
C* K. Brill/NMC          8/90   Fix lat-lon range problem		*
C* K. Brill/NMC		 7/91	Remove former case for lat > 90.	*
C* A. Taylor/ARL	12/93   Relax lat bound check for oblique azm   *
C* K. Brill/NMC		 1/94	Cleanup					*
C* K. Brill/EMC		 3/96	Changes for general rotated projection	*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	LOGICAL		maptyp
C------------------------------------------------------------------------
C*	Get the projection type and bounds for the correct coordinate
C*	system.
C
	IF  ( maptyp )  THEN
	    mtmtyp = 2
	    mmproj = mproj
	    cclatn = clatn
	    cclats = clats
	    cclonw = clonw
	    IF ( cclonw .gt. clone ) cclonw = clonw - 360.
	    cclone = clone
	    cangl1 = angle1
	    cangl2 = angle2
	    cangl3 = angle3
	  ELSE
	    jtmtyp = 2
	    mmproj = mgproj
	    cclatn = gclatn
	    cclats = gclats
	    cclonw = gclonw
	    IF ( cclonw .gt. gclone ) cclonw = gclonw - 360.
	    cclone = gclone
	    cangl1 = gangl1
	    cangl2 = gangl2
	    cangl3 = gangl3
	END IF
C
C*	Check that projection type is valid.
C
	IF ( ( mmproj .eq. MPAEQU ) .or. ( mmproj .eq. MPASTR ) .or.
     +	     ( mmproj .eq. MPAORT ) .or. ( mmproj .eq. MPALAM ) .or.
     +	     ( mmproj .eq. MPAGNO ) )  THEN
	    iret = NORMAL
	  ELSE
	    iret = NIPROJ
	    RETURN
	END IF
C
C*	Check for center at north or south pole.
C
	IF ((cangl1 .gt. 90.) .or. (cangl1 .lt. -90.)) THEN
 	    iret = NIPROJ
 	    RETURN
	ELSE IF  ( cangl1 .ge. 0. )  THEN
	    IF  ( maptyp )  THEN
		msppj  = MSANOR
	    ELSE
		mgsppj = MSANOR
	    END IF
	    piovr2 = HALFPI
	ELSE IF  ( cangl1 .lt. 0. )  THEN
	    IF  ( maptyp )  THEN
		msppj  = MSASOU
	    ELSE
		mgsppj = MSASOU
	    END IF
	    piovr2 = -HALFPI
	END IF
C
C*	Check for pole-centered map; then, clonw becomes polon.
C
	IF  ( ( cclonw .eq. cclone ) .and. ( cclats .eq. cclatn )
     +	    )  THEN
C
C*	    Set polon.
C
	    polon  = cclonw
	    polonr = polon * DTR
	    IF  ( maptyp )  THEN
		angle2 = polon
		anglr2 = polonr
	      ELSE
		gangl2 = polon
		gangr2 = polonr
	    END IF
	END IF
C
C*	Generate the M -> W and M -> Q rotation matrices.
C
C*	The central longitude of an unrotated map or grid is
C*	-90 deg.
C
	IF ( maptyp ) THEN
	    phi0 = piovr2 - anglr1
	    rlmda0 = anglr2 + HALFPI
	    theta0 = anglr3
	ELSE
	    phi0 = piovr2 - gangr1
	    rlmda0 = gangr2 + HALFPI
	    theta0 = gangr3
	END IF
	IF ( ABS ( phi0 ) .lt. 1.E-5 ) phi0 = 0.0
	IF ( ABS ( rlmda0 ) .lt. 1.E-5 ) rlmda0 = 0.0
	IF ( ABS ( theta0 ) .lt. 1.E-5 ) theta0 = 0.0
C*
	IF ( phi0 .eq. 0.0 .and. theta0 .eq. 0.0 ) THEN
C
C*	    Set first matrix element to simple longitude
C*	    rotation angle in radians.
C
	    DO i = 1, 3
		DO j = 1, 3
		    IF ( maptyp ) THEN
			am2w (i,j) = RMISSD
		    ELSE
			gm2q (i,j) = RMISSD
		    END IF
		END DO
	    END DO
	    IF ( maptyp ) THEN
		am2w (1,1) = rlmda0
		mtmtyp = 1
	    ELSE
		gm2q (1,1) = rlmda0
		jtmtyp = 1
	    END IF
	ELSE
            cosl = COS ( rlmda0 )
	    sinl = SIN ( rlmda0 )
	    cosp = COS ( phi0 )
	    sinp = SIN ( phi0 )
	    cost = COS ( theta0 )
	    sint = SIN ( theta0 )
C*
	    IF ( maptyp ) THEN
		am2w (1,1) = (-sint) * cosp * sinl + cost * cosl
		am2w (1,2) = sint * cosp * cosl + cost * sinl
		am2w (1,3) = sint * sinp
		am2w (2,1) = (-cost) * cosp * sinl - sint * cosl
		am2w (2,2) = cost * cosp * cosl - sint * sinl
		am2w (2,3) = cost * sinp
		am2w (3,1) = sinp * sinl
		am2w (3,2) = (-sinp) * cosl
		am2w (3,3) = cosp
	    ELSE
		gm2q (1,1) = (-sint) * cosp * sinl + cost * cosl
		gm2q (1,2) = sint * cosp * cosl + cost * sinl
		gm2q (1,3) = sint * sinp
		gm2q (2,1) = (-cost) * cosp * sinl - sint * cosl
		gm2q (2,2) = cost * cosp * cosl - sint * sinl
		gm2q (2,3) = cost * sinp
		gm2q (3,1) = sinp * sinl
		gm2q (3,2) = (-sinp) * cosl
		gm2q (3,3) = cosp
	    END IF
	END IF
C
C*	Find the range of linear intermediate coordinates for azimuthal
C*	projections.
C
	IF  ( maptyp )  THEN
	    IF  ( mmproj .eq. MPAORT )  THEN
		ueps = 1.
		veps = 1.
	      ELSE IF  ( mmproj .eq. MPAEQU )  THEN
		ueps = PI
		veps = PI
	      ELSE
		ueps = 2.
		veps = 2.
	    END IF
	END IF
C
C*	Check for special case of projection centered on pole.
C*	In this case clonw = clone and clats = clatn.
C
	IF  ( ( cclonw .eq. cclone ) .and. ( cclats .eq. cclatn )
     +	    ) THEN
C
C*	    Compute range of linear coordinates in the rotated
C*	    lat/lon coordinate.
C
	    poln = -90.
	    polop = poln + 180.
	    CALL PRNLON ( 1, polop, ier )
	    pleft = poln - 90.
	    CALL PRNLON ( 1, pleft, ier )
	    prght = poln + 90.
	    CALL PRNLON ( 1, prght, ier )
C
C*	    Get corners in linear intermediate coordinates.
C
	    IF  ( maptyp )  THEN
		mset  = .true.
		IF  ( msppj .eq. MSASOU )  THEN
		  CALL GTRANS  ( 'W', 'L', 1, cclats, poln,
     +				 xbot, ytop, ier1 )
		  CALL GTRANS  ( 'W', 'L', 1, cclats, polop,
     +				 xtop, ybot, ier2 )
		 ELSE
		  CALL GTRANS  ( 'W', 'L', 1, cclats, poln,
     +				 xbot, ybot, ier1 )
		  CALL GTRANS  ( 'W', 'L', 1, cclats, polop,
     +				 xtop, ytop, ier2 )
		END IF
		CALL GTRANS  ( 'W', 'L', 1, cclats, pleft,
     +			       xlft, ylft, ier3 )
		CALL GTRANS  ( 'W', 'L', 1, cclats, prght,
     +			       xrgt, yrgt, ier4 )
	      ELSE
		mgset  = .true.
		IF  ( mgsppj .eq. MSASOU )  THEN
		  CALL GTRANS  ( 'Q', 'I', 1, cclats, poln,
     +				 xbot, ytop, ier1 )
		  CALL GTRANS  ( 'Q', 'I', 1, cclats, polop,
     +				 xtop, ybot, ier2 )
		 ELSE
		  CALL GTRANS  ( 'Q', 'I', 1, cclats, poln,
     +				 xbot, ybot, ier1 )
		  CALL GTRANS  ( 'Q', 'I', 1, cclats, polop,
     +				 xtop, ytop, ier2 )
		END IF
		CALL GTRANS  ( 'Q', 'I', 1, cclats, pleft,
     +			       xlft, ylft, ier3 )
		CALL GTRANS  ( 'Q', 'I', 1, cclats, prght,
     +			       xrgt, yrgt, ier4 )
	    END IF
C
C*	    Check errors.
C
	    ier = ier1 + ier2 + ier3 + ier4
	    IF  ( ier .ne. 0 )  THEN
		iret = NIPBND
		RETURN
	    END IF
C
C*	    Set linear intermediate bounds.  Note that in grid
C*	    coordinates, no further calculations need to be performed.
C
	    IF  ( maptyp )  THEN
		xmbndl = xlft
		ymbndb = ybot
		xmbndr = xrgt
		ymbndt = ytop
	      ELSE
		gxbndl = xlft
		gybndb = ybot
		gxbndr = xrgt
		gybndt = ytop
		RETURN
	    END IF
	  ELSE
C
C*	    Compute the bounds for the case where the corners are
C*	    exact.
C
	    IF  ( maptyp )  THEN
		mset  = .true.
C
C*		Find linear intermediate values at corners.
C
		CALL GTRANS  ( 'M', 'L', 1, cclats, cclonw,
     +			       xlll, ylll, ier1 )
		CALL GTRANS  ( 'M', 'L', 1, cclatn, cclone,
     +			       xlur, ylur, ier2 )
	      ELSE
		mgset  = .true.
C
C*		Find linear intermediate values at corners.
C
		CALL GTRANS  ( 'M', 'I', 1, cclats, cclonw,
     +			       xlll, ylll, ier1 )
		CALL GTRANS  ( 'M', 'I', 1, cclatn, cclone,
     +			       xlur, ylur, ier2 )
	    END IF
C
C*	    Check that the coordinates are valid.
C
	    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +		  ( xlll .ge. xlur ) .or. ( ylll .ge. ylur ) )  THEN
		iret  = NIPBND
		RETURN
	    END IF
C
C*	    Set linear intermedidate bounds.  Return here for grid 
C*	    coordinates.
C
	    IF  ( maptyp )  THEN
		xmbndl = xlll
		ymbndb = ylll
		xmbndr = xlur
		ymbndt = ylur
	      ELSE
		gxbndl = xlll
		gybndb = ylll
		gxbndr = xlur
		gybndt = ylur
		RETURN
	    END IF
C*
	END IF
C
C*	Now, find the lat/lon range covered by the plotting
C*	area.
C
	IF ( maptyp ) THEN
	    CALL UDMBND ( xmbndl, ymbndb, xmbndr, ymbndt,
     +			  blats, blonw, blatn, blone, ier )
	    IF ( ier .ne. NORMAL ) THEN
		blats = -90.
		blatn = +90.
		blonw = -180.
		blone = +180.
	    END IF
	END IF
C*
	RETURN
	END
