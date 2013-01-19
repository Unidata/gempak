	SUBROUTINE UPDCON  ( maptyp, iret )
C************************************************************************
C* UPDCON								*
C*									*
C* This subroutine updates the map common area when conic 		*
C* coordinates are defined.  This subroutine may be used to set up	*
C* either the map or grid linear coordinate system.  For the map	*
C* coordinate setup, maptyp must be true; for grid coordinates,		*
C* maptyp must be false.						*
C*									*
C* UPDCON  ( MAPTYP, IRET )						*
C*									*
C* Input parameters:							*
C*	MAPTYP		LOGICAL		Map type			*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC							*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* K. Brill/GSC          1/90   Check for dateline crossing		*
C* K. Brill/GSC          2/90   Corrected pole problems			*
C* K. Brill/NMC          8/90   Correct lat-lon range problem		*
C* K. Brill/NMC   	10/90   Correct PLAT3 to PLAT2 in grid code	*
C* K. Brill/NMC		 7/91	Remove former case for lat > 90.	*
C* K. Brill/EMC		 3/96	Changes for general rotation		*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	LOGICAL		maptyp
C-------------------------------------------------------------------------
C*	Get the projection type and bounds for the correct coordinate
C*	system.
C
	IF  ( maptyp )  THEN
	    mtmtyp = 2
	    mmproj = mproj
	    cclatn = clatn
	    cclats = clats
	    cclonw = clonw
	    IF ( clonw .gt. clone ) cclonw = clonw - 360.
	    cclone = clone
	    plat1  = anglr1
	    plat2  = anglr3
	    cangl1 = angle1
	    cangl2 = angle2
	    cangl3 = angle3
	  ELSE
	    jtmtyp = 2
	    mmproj = mgproj
	    cclatn = gclatn
	    cclats = gclats
	    cclonw = gclonw
            IF ( gclonw .gt. gclone ) cclonw = gclonw - 360.
	    cclone = gclone
	    plat1  = gangr1
	    plat2  = gangr3
	    cangl1 = gangl1
	    cangl2 = gangl2
	    cangl3 = gangl3
	END IF
C
C*	Set rotation angle (deg).
C
	rotat = 0.0
C
C*	Check for valid projection type.
C
	IF  ( mmproj .eq. MPCNOR )  THEN
	    iret = NORMAL
	    IF  ( maptyp )  THEN
		msppj  = MSANOR
	      ELSE 
		mgsppj = MSANOR
	    END IF
	    polat = 90.
	    piovr2 = HALFPI
	  ELSE IF  ( mmproj .eq. MPCSOU )  THEN
	    iret = NORMAL
	    IF  ( maptyp )  THEN
		msppj  = MSASOU
	      ELSE
		mgsppj = MSASOU
	    END IF
	    polat = -90.
	    piovr2 = -HALFPI
	  ELSE
	    iret = NIPROJ
	    RETURN
	END IF
C
C*	Check for special case of projection centered on pole.
C*	In this case clonw = clone and clats = clatn.
C*	Then, clonw becomes polon.
C
	IF  (( cclonw .eq. cclone ) .and. ( cclats .eq. cclatn )
     +	     .and. ( polat .eq. 90. .or. polat .eq. -90. ) )  THEN
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
C*	Compute the cone constant for conic projections.
C
	IF  ( ( ABS ( plat1 ) .eq. HALFPI ) .or. 
     +	      ( ABS ( plat2 ) .eq. HALFPI ) )  THEN
	    iret = NIPROJ
	    RETURN
	  ELSE IF  ( plat1 .eq. plat2 )  THEN
	    sgn   = SIGN ( 1., plat1 )
	    cona  = HALFPI - sgn * plat1
	    cone  = COS  ( cona )
	  ELSE
	    cona = ALOG ( COS ( plat1 ) ) - ALOG ( COS ( plat2 ) )
	    IF  ( mmproj .eq. MPCNOR )  THEN
		conb = ALOG ( TAN ( PI4TH - plat1 / 2.0 ) )
		conc = ALOG ( TAN ( PI4TH - plat2 / 2.0 ) )
	      ELSE IF  ( mmproj .eq. MPCSOU )  THEN
		conb = ALOG ( TAN ( PI4TH + plat1 / 2.0 ) )
		conc = ALOG ( TAN ( PI4TH + plat2 / 2.0 ) )
	    END IF
	    cone = cona / ( conb - conc )
	END IF
C
C*	The general Conic transformation requires five angles.
C*	The current setup permits only three; so, the rotation
C*	angles POLAT and ROTAT are set above.  At some point in
C*	the future the general rotation can be added.  Then, the
C*	true latitudes, of course, will apply in the rotated frame.
C*	The need to always specify the true latitudes (to compute a
C*	cone constant) raises the required number of angles to five.
C
C*	It may never be desireable to add the general rotation
C*	because the location of the cut follows the pole.
C
C*	Generate the M -> W and M -> Q rotation matrices.
C
C*	The central longitude of an unrotated map or grid is
C*	-90 deg.
C
	IF ( maptyp ) THEN
	    phi0 = piovr2 - polat * DTR
	    rlmda0 = anglr2 + HALFPI
	    theta0 = rotat * DTR
	ELSE
	    phi0 = piovr2 - polat * DTR
	    rlmda0 = gangr2 + HALFPI
	    theta0 = rotat * DTR
	END IF
        IF ( ABS ( phi0 ) .lt. 1.E-5 ) phi0 = 0.0
        IF ( ABS ( rlmda0 ) .lt. 1.E-5 ) rlmda0 = 0.0
        IF ( ABS ( theta0 ) .lt. 1.E-5 ) theta0 = 0.0
C*
        IF ( phi0 .eq. 0.0 .and. theta0 .eq. 0.0 ) THEN
C
C*          Set first matrix element to simple longitude
C*          rotation angle in radians.
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
C*	Save cone constant in common.
C*	Also save range of intermedidate coordinates for map projection.
C
	IF  ( maptyp )  THEN
	    concon = cone
	    ueps = 1.
	    veps = 1.
	  ELSE
	    gconcn = cone
	END IF
C*
	IF  (( cclonw .eq. cclone ) .and. ( cclats .eq. cclatn )
     +	     .and. ( polat .eq. 90. .or. polat .eq. -90. ) )  THEN
C
C*	    Compute range of linear coordinates for case when 
C*	    the pole is at the center.
C
	    polop = polon + 180.
	    CALL PRNLON ( 1, polop, ier )
	    pleft = polon - 90./cone
	    CALL PRNLON ( 1, pleft, ier )
	    prght = polon + 90./cone
	    CALL PRNLON ( 1, prght, ier )
C
C*	    Get corners in linear intermediate coordinates.
C
	    IF  ( maptyp )  THEN
		mset  = .true.
		CALL GTRANS  ( 'M', 'L', 1, cclats, polon,
     +			      xbot, ybot, ier1 )
		CALL GTRANS  ( 'M', 'L', 1, cclats, polop,
     +			      xtop, ytop, ier2 )
		CALL GTRANS  ( 'M', 'L', 1, cclats, pleft,
     +			      xlft, ylft, ier3 )
		CALL GTRANS  ( 'M', 'L', 1, cclats, prght,
     +			      xrgt, yrgt, ier4 )
	      ELSE
		mgset  = .true.
		CALL GTRANS  ( 'M', 'I', 1, cclats, polon,
     +			      xbot, ybot, ier1 )
		CALL GTRANS  ( 'M', 'I', 1, cclats, polop,
     +			      xtop, ytop, ier2 )
		CALL GTRANS  ( 'M', 'I', 1, cclats, pleft,
     +			      xlft, ylft, ier3 )
		CALL GTRANS  ( 'M', 'I', 1, cclats, prght,
     +			      xrgt, yrgt, ier4 )
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
C
C*	    Now, set the lat/lon range covered by the plotting
C*	    area.
C
	    IF  ( mmproj .eq. MPCNOR )  THEN
		blats = cclats
		blatn = 90.
	      ELSE
		blats = -90.
		blatn = cclatn
	    END IF
	    blonw = -180.
	    blone =  180.
	    RETURN
	END IF
C
C*	Now, do computations where the corners have been input.
C*	Compute bounds in linear intermediate coordinates.
C*	Convert lat/lon corners to linear coordinates.
C
	IF  ( maptyp )  THEN
	    mset  = .true.
	    CALL GTRANS  ( 'M', 'L', 1, cclats, cclonw,
     +			   xlll, ylll, ier1 )
	    CALL GTRANS  ( 'M', 'L', 1, cclatn, cclonw,
     +			   xlul, ylul, ier2 )
	    CALL GTRANS  ( 'M', 'L', 1, cclats, cclone,
     +			   xllr, yllr, ier3 )
	    CALL GTRANS  ( 'M', 'L', 1, cclatn, cclone,
     +			   xlur, ylur, ier4 )
	  ELSE
	    mgset  = .true.
	    CALL GTRANS  ( 'M', 'I', 1, cclats, cclonw,
     +			   xlll, ylll, ier1 )
	    CALL GTRANS  ( 'M', 'I', 1, cclatn, cclone,
     +			   xlur, ylur, ier4 )
	    ier2 = 0
 	    ier3 = 0
	END IF
C
C*	Check for errors and save values in common.
C
	ier = ier1 + ier2 + ier3 + ier4
	IF  ( ( ier .ne. 0 ) .or. ( xlll .ge. xlur ) .or.
     +	      ( ylll .ge. ylur ) )  THEN
	    iret = NIPBND
	    RETURN
	  ELSE
	    IF  ( maptyp )  THEN
		xmbndl = xlll
		xmbndr = xlur
		ymbndb = ylll
		ymbndt = ylur
	      ELSE
		gxbndl = xlll
		gxbndr = xlur
		gybndb = ylll
		gybndt = ylur
		RETURN
	    END IF
	END IF
C
C*	Now compute limits of the map area which will be displayed.
C
	IF ( maptyp ) THEN
            CALL UDMBND ( xmbndl, ymbndb, xmbndr, ymbndt,
     +                    blats, blonw, blatn, blone, ier )
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
