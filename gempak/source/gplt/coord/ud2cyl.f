	SUBROUTINE UD2CYL  ( maptyp, iret )
C************************************************************************
C* UD2CYL								*
C*									*
C* This subroutine updates the map common area when cylindrical map 	*
C* coordinates are defined.  This subroutine is used when UPDCYL	*
C* fails because the cylinder axis is such that rotating the x-y	*
C* coordinates is necessary to develop a transverse projection		*
C* under the GEMPLT linear coordinate constraints.			*
C*									*
C* UD2CYL ( MAPTYP, IRET )						*
C*									*
C* Input parameters:							*
C*	MAPTYP		LOGICAL		Map coordinate flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/EMC		 3/96	Made from UPDCYL			*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	LOGICAL		maptyp
C*
	INCLUDE		'ERMISS.FNC'
C-------------------------------------------------------------------------
	iret = NORMAL
C
C*	Get the projection type and bounds for the correct coordinate
C*	system.
C
	IF  ( maptyp )  THEN
	    mmproj = mproj
	    cclatn = clatn
	    cclats = clats
	    cclonw = clonw
	    cclone = clone
	  ELSE
	    mmproj = mgproj
	    cclatn = gclatn
	    cclats = gclats
	    cclonw = gclonw
	    cclone = gclone
	END IF
C
C*	Compute bounds in linear intermediate coordinates.
C
	IF  ( maptyp )  THEN
	    mset  = .true.
C
C*	    Convert lat-lon corners to linear coordinates.
C
	    CALL GTRANS ( 'M', 'L', 1, cclats, cclonw,
     +			 xlll, ylll, ier1 )
	    CALL GTRANS ( 'M', 'L', 1, cclatn, cclone,
     +			 xlur, ylur, ier2 )
	  ELSE
	    mgset = .true.
C
C*	    Convert lat-lon corners to linear coordinates.
C
	    CALL GTRANS ( 'M', 'I', 1, cclats, cclonw,
     +			 xlll, ylll, ier1 )
	    CALL GTRANS ( 'M', 'I', 1, cclatn, cclone,
     +			 xlur, ylur, ier2 )
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
C*	Return for invalid coordinate system.
C
	IF  ( iret .ne. NORMAL )  RETURN
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
		RETURN
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
		RETURN
	    END IF
	END IF
C*
	RETURN
	END
