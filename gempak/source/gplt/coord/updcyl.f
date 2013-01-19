	SUBROUTINE UPDCYL  ( maptyp, iret )
C************************************************************************
C* UPDCYL								*
C*									*
C* This subroutine updates the map common area when cylindrical map 	*
C* coordinates are defined.  This subroutine may be used to set up	*
C* either the map or grid linear coordinate system.  For the map	*
C* coordinate setup, maptyp must be true; for grid coordinates,		*
C* maptyp must be false.						*
C*									*
C* A rotated full globe CED map can be setup.  The central longitude of	*
C* the map will be POLON.  The map bounds must be specified so that the *
C* north and south latitude bounds are zero.  The  longitude bounds	*
C* are ignored, but must have equal values.				*
C*									*
C* UPDCYL ( MAPTYP, IRET )						*
C*									*
C* Input parameters:							*
C*	MAPTYP		LOGICAL		Map coordinate flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC							*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* K. Brill/GSC          1/90   Check for dateline crossing (ktype = 2 )*
C* J. Cowie/COMET       12/94   Added angle1 check for MCD - lon/lat    *
C*                              aspect ratio.                           *
C* K. Brill/EMC		 3/96	Changes for general rotated projection	*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* K. Brill/EMC		 6/98	Set rotation for L coords for WAFS	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	LOGICAL		maptyp
C*
	LOGICAL		reset2, angchk
	INCLUDE		'ERMISS.FNC'
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
	    cclone = clone
	    mrvrxy = .false.
	    cangl1 = angle1
	    cangl2 = angle2
	    IF ( angle3 .gt. 360 ) THEN
		rotaml = .true.
		alphml = ( angle3 - 360.0 ) * DTR
		angle3 = 0.0
		anglr3 = 0.0
	    ELSE IF ( angle3 .lt. -360 ) THEN
		rotaml = .true.
		alphml = ( angle3 + 360.0 ) * DTR
		angle3 = 0.0
		anglr3 = 0.0
	    ELSE
		rotaml = .false.
		alphml = 0.0
	    END IF
	    cangl3 = angle3
	    rcosml = COS ( alphml )
	    rsinml = SIN ( alphml )
	  ELSE
	    jtmtyp = 2
	    mmproj = mgproj
	    cclatn = gclatn
	    cclats = gclats
	    cclonw = gclonw
	    cclone = gclone
	    grvrxy = .false.
	    cangl1 = gangl1
	    cangl2 = gangl2
	    IF ( gangl3 .gt. 360 ) THEN
		rotagl = .true.
		alphgl = ( gangl3 - 360.0 ) * DTR
		gangl3 = 0.0
		gangr3 = 0.0
	    ELSE IF ( gangl3 .lt. -360 ) THEN
		rotagl = .true.
		alphgl = ( gangl3 + 360.0 ) * DTR
		gangl3 = 0.0
		gangr3 = 0.0
	    ELSE
		rotagl = .false.
		alphgl = 0.0
	    END IF
	    cangl3 = gangl3
	    rcosgl = COS ( alphgl )
	    rsingl = SIN ( alphgl )
	END IF
C
C*	Check for valid projection type.
C
	IF  ( ( mmproj .eq. MPCEQU ) .or. ( mmproj .eq. MPCMER ) .or.
     +	      ( mmproj .eq. MPCMCD ) )  THEN
	    iret = NORMAL
	  ELSE
	    iret = NIPROJ
	    RETURN
	END IF
C
C*	Save scaling term for MCD projections.  The scaling term will
C*	be a lon/lat aspect ratio if angle1 is nonzero, otherwise a
C*	function of latitude.
C
	IF  ( mmproj .eq. MPCMCD )  THEN
	    avlat = ( cclatn + cclats ) / 2.0
	    IF  ( ( avlat .eq. 90.0 ) .or. ( avlat .eq. -90.0 ) )  THEN
		iret = NIPBND
		RETURN
	    END IF
	    IF ( cangl1 .ne. 0.0 ) THEN
		sterm = cangl1
	    ELSE
	        sterm = 1.0 / COS ( avlat * DTR )
	    END IF
	    IF  ( maptyp )  THEN
		sclmcd = sterm
	      ELSE
		gscmcd = sterm
	    END IF
	END IF
C
C*	Set angle2 (POLON) so that it is reasonable for entire
C*	earth case.
C
	angchk = ( cangl1 .eq. 0.0 .and. cangl2 .eq. 0.0 .and.
     +		   cangl3 .eq. 0.0 )
	IF ( angchk ) THEN
	    IF  ( cclonw .eq. cclone )  THEN
		polon = cclonw + 180.0
		CALL PRNLON ( 1, polon, ier )
	    ELSE IF  ( cclonw .gt. cclone )  THEN
		polon = ( 360.0 + cclonw + cclone ) / 2.0
		CALL PRNLON ( 1, polon, ier )
	    ELSE
		polon = ( cclonw + cclone ) / 2.0
		CALL PRNLON ( 1, polon, ier )
	    END IF
	    reset2 = .true.
	ELSE
	    reset2 = .false.
	END IF
C
C*	Replace values in common.
C
	IF  ( maptyp .and. reset2 )  THEN
	    angle2 = polon
	    anglr2 = polon * DTR
	  ELSE IF ( reset2 ) THEN
	    gangl2 = polon
	    gangr2 = polon * DTR
	END IF
C
C*	Generate the M -> W and M -> Q rotation matrices.
C
	IF ( maptyp ) THEN
	    phi0 = anglr1
	    rlmda0 = anglr2
	    theta0 = anglr3
	ELSE
	    phi0 = gangr1
	    rlmda0 = gangr2
	    theta0 = gangr3
	END IF
C
C*	For MCD, allow only longitude rotation.  (Angle 1 is
C*	the scaling factor.)
C
	IF  ( mmproj .eq. MPCMCD )  THEN
	    phi0 = 0.0
	    theta0 = 0.0
	END IF
C*
	IF ( ABS ( phi0 ) .lt. 1.E-5 ) phi0 = 0.0
	IF ( ABS ( rlmda0 ) .lt. 1.E-5 ) rlmda0 = 0.0
	IF ( ABS ( theta0 ) .lt. 1.E-5 ) theta0 = 0.0
C*
	IF ( phi0 .eq. 0.0 .and. theta0 .eq. 0.0 ) THEN
C
C*	Set first matrix element to simple longitude
C*	rotation angle in radians.
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
		am2w (1,1) = cosp * cosl
		am2w (1,2) = cosp * sinl
		am2w (1,3) = sinp
		am2w (2,1) = (-cost) * sinl - sint * sinp * cosl
		am2w (2,2) = cost * cosl - sint * sinp * sinl
		am2w (2,3) = sint * cosp
		am2w (3,1) = sint * sinl - cost * sinp * cosl
		am2w (3,2) = (-sint) * cosl - cost * sinp * sinl
		am2w (3,3) = cost * cosp
	    ELSE
		gm2q (1,1) = cosp * cosl
		gm2q (1,2) = cosp * sinl
		gm2q (1,3) = sinp
		gm2q (2,1) = (-cost) * sinl - sint * sinp * cosl
		gm2q (2,2) = cost * cosl - sint * sinp * sinl
		gm2q (2,3) = sint * cosp
		gm2q (3,1) = sint * sinl - cost * sinp * cosl
		gm2q (3,2) = (-sint) * cosl - cost * sinp * sinl
		gm2q (3,3) = cost * cosp
	    END IF
	END IF
C
C*	Set ups and veps which are distances across earth.
C
	IF  ( maptyp )  THEN
	    ueps = PI
	    veps = HALFPI
	    IF  ( mmproj .eq. MPCMER )  veps = PI
	END IF
C
C*	Compute bounds in linear intermediate coordinates.
C
	IF  ( maptyp )  THEN
	    mset  = .true.
	    IF ( ( cclats .eq. 0.0 .and. cclatn .eq. 0.0 )
     +				  .and.
     +		   cclonw .eq. cclone ) THEN
C
C*		Setup total globe in W coordinates.
C
		CALL GTRANS ( 'W', 'L', 1, -90.0, angle2,
     +			 	xlll, ylll, ier1 )
		CALL GTRANS ( 'W', 'L', 1,  90.0, angle2,
     +			 	xlur, ylur, ier2 )
	    ELSE
C
C*		Convert lat-lon corners to linear coordinates.
C
		CALL GTRANS ( 'M', 'L', 1, cclats, cclonw,
     +			 	xlll, ylll, ier1 )
		CALL GTRANS ( 'M', 'L', 1, cclatn, cclone,
     +			 	xlur, ylur, ier2 )
		IF ( xlur .lt. xlll .and. mtmtyp .eq. 2 ) THEN
C
C*		    Reset angle 2 to use in the rotated coordinate.
C
		    CALL GTRANS ( 'M', 'W', 1, cclats, cclonw,
     +			      qlat, qclonw, ier )
		    CALL GTRANS ( 'M', 'W', 1, cclatn, cclone,
     +			      qlat, qclone, ier )
		    IF  ( qclonw .eq. qclone )  THEN
			polon = qclonw + 180.0
			CALL PRNLON ( 1, polon, ier )
		    ELSE IF  ( qclonw .gt. qclone )  THEN
			polon = ( 360.0 + qclonw + qclone ) / 2.0
			CALL PRNLON ( 1, polon, ier )
		    ELSE
			polon = ( qclonw + qclone ) / 2.0
			CALL PRNLON ( 1, polon, ier )
		    END IF
		    angle2 = polon
		    anglr2 = polon * DTR
		    mtmtyp = 3
		    CALL GTRANS ( 'M', 'L', 1, cclats, cclonw,
     +			      xlll, ylll, ier1 )
		    CALL GTRANS ( 'M', 'L', 1, cclatn, cclone,
     +			      xlur, ylur, ier2 )
	        END IF
	    END IF
	  ELSE
	    mgset = .true.
C
C*	    Convert lat-lon corners to grid linear coordinates.
C
	    CALL GTRANS ( 'M', 'I', 1, cclats, cclonw,
     +			 xlll, ylll, ier1 )
	    CALL GTRANS ( 'M', 'I', 1, cclatn, cclone,
     +			 xlur, ylur, ier2 )
	    IF ( xlur .lt. xlll .and. jtmtyp .eq. 2 ) THEN
C
C*		Reset angle 2 to use in the rotated coordinate.
C
		CALL GTRANS ( 'M', 'Q', 1, cclats, cclonw,
     +			      qlat, qclonw, ier )
		CALL GTRANS ( 'M', 'Q', 1, cclatn, cclone,
     +			      qlat, qclone, ier )
		IF  ( qclonw .eq. qclone )  THEN
		    polon = qclonw + 180.
		    CALL PRNLON ( 1, polon, ier )
		ELSE IF  ( qclonw .gt. qclone )  THEN
		    polon = ( 360. + qclonw + qclone ) / 2.0
		    CALL PRNLON ( 1, polon, ier )
		ELSE
		    polon = ( qclonw + qclone ) / 2.0
		    CALL PRNLON ( 1, polon, ier )
		END IF
		gangl2 = polon
		gangr2 = polon * DTR
		jtmtyp = 3
		CALL GTRANS ( 'M', 'I', 1, cclats, cclonw,
     +			      xlll, ylll, ier1 )
		CALL GTRANS ( 'M', 'I', 1, cclatn, cclone,
     +			      xlur, ylur, ier2 )
	    END IF
	END IF
	IF ( ERMISS ( xlll ) .or. ERMISS ( xlur ) ) THEN
	    iret = NIPBND
	    RETURN
	END IF
C
C*	Check for errors in transformations.
C
	ier = ier1 + ier2
	IF  ( ier .ne. NORMAL )  iret = NIPBND
C
C*	Check that lower y is less than upper y.
C
	IF  ( ylll .ge. ylur )  iret = NIPBND
C
C*	Save values in common.
C
	IF  ( maptyp )  THEN
	    ymbndb = ylll
	    ymbndt = ylur
C
C*	    Set linear intermediate coordinate bounds.
C
	    IF  ( xlll .eq. xlur )  THEN
		xmbndl = -PI
		xmbndr = +PI
	      ELSE IF ( xlll .lt. xlur ) THEN
		xmbndl = xlll
		xmbndr = xlur
	      ELSE 
		iret = NIPBND
	    END IF
	    IF ( iret .eq. NIPBND .and. mmproj .ne. MPCMCD ) THEN
C
C*		Rotate the linear coordinates thru -90 so that
C*		the x-axis is parallel to the cylinder axis 
C*		for a transverse projection.
C
		mrvrxy = .true.
		CALL UD2CYL  ( maptyp, iret )
	    END IF
	  ELSE
C
C*	    Save values for grid transformations.
C
	    gybndb = ylll
	    gybndt = ylur
C
C*	    Set linear intermediate coordinate bounds.
C
	    IF  ( xlll .eq. xlur )  THEN
		gxbndl = -PI
		gxbndr = +PI
	      ELSE IF ( xlll .lt. xlur ) THEN
		gxbndl = xlll
		gxbndr = xlur
	      ELSE 
		iret = NIPBND
	    END IF
	    IF ( iret .eq. NIPBND .and. mmproj .ne. MPCMCD ) THEN
C
C*		Rotate the linear coordinates thru -90 so that
C*		the x-axis is parallel to the cylinder axis 
C*		for a transverse projection.
C
		grvrxy = .true.
		CALL UD2CYL  ( maptyp, iret )
	    END IF
	END IF
C*
	IF ( maptyp .and. iret .eq. NORMAL ) THEN
C
C*	    Search all lat/lons for arbitrary rotated projection.
C
            CALL UDMBND ( xmbndl, ymbndb, xmbndr, ymbndt,
     +                    blats, blonw, blatn, blone, ier )
	    IF ( ier .ne. NORMAL ) THEN
		blats = -90.0
		blatn =  90.0
		blonw = -180.0
		blone =  180.0
	    END IF
	END IF
C*
	RETURN
	END
