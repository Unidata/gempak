	SUBROUTINE GSMPRJ  (  proj,  angl1,  angl2,  angl3,
     +			     dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GSMPRJ								*
C*									*
C* This subroutine provides a general way to specify the map projection	*
C* and bounds to be used when plotting in map coordinates.  The		*
C* subroutine now uses a character projection name rather than map 	*
C* class and projection numbers.  The valid projection names are:	*
C*									*
C*            CLASS      PROJ        PROJECTION				*
C*									*
C*      Cylindrical       CED        Cylindrical equidistant		*
C*                        MER        Mercator				*
C*                        MCD        Modified cylind. equidistant	*
C*									*
C*        Azimuthal       AED        Azimuthal equidistant		*
C*                        STR        Stereographic			*
C*                        ORT        Orthographic			*
C*                        LEA        Lambert equal area			*
C*                        GNO        Gnomonic				*
C*									*
C*          Conical       LCC        North Lambert conic conformal	*
C*                        SCC        South Lambert conic conformal	*
C*									*
C* Oblique Mercator       TVM        Transverse Mercator		*
C*                        UTM        Universal Transverse Mercator	*
C*									*
C* For the UTM projection, the tangential longitude is adjusted to	*
C* a standard longitude by rounding down to ..., -9, -3, 3, 9, ...	*
C*									*
C* The angles are defined for the various map classes.			*
C*									*
C*      Cylindrical       angle1 -- latitude of origin on projection	*
C*				    cylinder (0 = Equator)		*
C*                        angle2 -- longitude of origin on projection	*
C*				    cylinder (0 = Greenwich meridian)	*
C*			  angle3 -- angle to rotate projection about a 	*
C*				    line from the Earth's center thru	*
C*				    the origin point.			*
C*									*
C*        Azimuthal       angle1 -- latitude of projection's point of	*
C*				    tangency (pole latitude)		*
C*                        angle2 -- longitude of projection's point of	*
C*				    tangency (the central longitude)	*
C*                        angle3 -- angle to rotate projection about a  *
C*				    line from the Earth's center thru	*
C*				    the point of tangency		*
C*									*
C*         Conical        angle1 -- standard latitude 1			*
C*                        angle2 -- polon (the central longitude)	*
C*                        angle3 -- standard latitude 2			*
C*									*
C*         Oblique        angle1 -- tangential longitude		*
C*                        angle2 -- not used				*
C*                        angle3 -- not used				*
C*									*
C* Azimuthal and conic projections which include the pole, can be 	*
C* defined by specifying the lower left and upper right corners.  If	*
C* DLATLL and DLATUR are equal and DLONLL and DLONUR are also equal,	*
C* a map area will be defined which includes the area from the pole 	*
C* to the given latitude in each direction with DLONLL as the central	*
C* longitude.								*
C*									*
C* GSMPRJ  ( PROJ, ANGL1, ANGL2, ANGL3, DLATLL, DLONLL, 		*
C*           DLATUR, DLONUR, IRET )					*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	ANGL1		REAL		Reference angle 1		*
C*	ANGL2		REAL		Reference angle 2		*
C*	ANGL3		REAL		Reference angle 3		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 3/82						*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed projection to character		*
C* K. Brill/NMC		07/91	Documentation changed			*
C* S. Jacobs/NMC	 1/95	Added multi-window common block		*
C* K. Brill/EMC		 6/98	Documentation				*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'DEVWIN.CMN'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	proj
C*
	CHARACTER	cprj*4
C------------------------------------------------------------------------
C*	Check that map mode has been set.
C
	IF  ( igmode .ne. 1 )  THEN
	    iret = NIMODE
	  ELSE
C
C*	    Save input values.
C
	    CALL ST_LCUC  ( proj, cprj, ier )
	    prjnam = cprj
	    IF  ( cprj .eq. 'CED' )  THEN
		mclass = 1
		mproj  = 1
	      ELSE IF  ( cprj .eq. 'MER' )  THEN
		mclass = 1
		mproj  = 2
	      ELSE IF  ( cprj .eq. 'MCD' )  THEN
		mclass = 1
		mproj  = 3
	      ELSE IF  ( cprj .eq. 'AED' )  THEN
		mclass = 2
		mproj  = 1
	      ELSE IF  ( cprj .eq. 'STR' )  THEN
		mclass = 2
		mproj  = 2
	      ELSE IF  ( cprj .eq. 'ORT' )  THEN
		mclass = 2
		mproj  = 3
	      ELSE IF  ( cprj .eq. 'LEA' )  THEN
		mclass = 2
		mproj  = 4
	      ELSE IF  ( cprj .eq. 'GNO' )  THEN
		mclass = 2
		mproj  = 5
	      ELSE IF  ( cprj .eq. 'LCC' )  THEN
		mclass = 3
		mproj  = 1
	      ELSE IF  ( cprj .eq. 'SCC' )  THEN
		mclass = 3
		mproj  = 2
	      ELSE IF  ( cprj .eq. 'TVM' )  THEN
		mclass = 4
		mproj  = 1
	      ELSE IF  ( cprj .eq. 'UTM' )  THEN
		mclass = 4
		mproj  = 2
	    END IF
	    msppj  = 0
	    mtype  = 2
	    angle1 = angl1
	    angle2 = angl2
	    angle3 = angl3
	    anglr1 = angle1 * DTR
	    anglr2 = angle2 * DTR
	    anglr3 = angle3 * DTR
C
C*	    Save bounds.
C
	    clats = dlatll
	    clonw = dlonll
	    clatn = dlatur
	    clone = dlonur
C
C*	    Set up map projection and fit on device.
C
	    CALL UPDMAP ( iret )
	    IF  ( iret .eq. NORMAL ) CALL UPDPXY
	END IF
C
C*	Check for valid map projection set.
C
	IF  ( iret .ne. NORMAL )  THEN
	    mset  = .false.
	    mtype = 0
	ELSE
C
C*	    Save common variables for window.
C
	    nmode  (ncurwn) = igmode
	    wcproj (ncurwn) = cprj
	    uangle (ncurwn,1) = angl1
	    uangle (ncurwn,2) = angl2
	    uangle (ncurwn,3) = angl3
	    ulatll (ncurwn) = dlatll
	    ulonll (ncurwn) = dlonll
	    ulatur (ncurwn) = dlatur
	    ulonur (ncurwn) = dlonur
	END IF
C*
	RETURN
	END
