	SUBROUTINE GSMPRJ  (  proj,  angle1, angle2, angle3,
     +			     dlatll, dlonll, dlatur, dlonur,  iret )
C************************************************************************
C* GSMPRJ								*
C*									*
C* This subroutine provides a general way to specify the map projection	*
C* and bounds to be used when plotting in map coordinates.  The valid   *
C* projection names are:						*
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
C*                        UTM        Universal transverse Mercator	*
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
C*				    the origin point 			*
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
C* Azimuthal and conic projections which include the pole are defined   *
C* by specifying the lower left and upper right corners.  If DLATLL and *
C* DLATUR are equal and DLONLL and DLONUR are also equal, a map area is *
C* defined which includes the area from the pole to the given latitude  *
C* in each direction with DLONLL as the central longitude.		*
C*									*
C* GSMPRJ  ( PROJ, ANGLE1, ANGLE2, ANGLE3, DLATLL, DLONLL, 		*
C*           DLATUR, DLONUR, IRET )					*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	ANGLE1		REAL		Reference angle 1		*
C*	ANGLE2		REAL		Reference angle 2		*
C*	ANGLE3		REAL		Reference angle 3		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/84	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Added character projection name		*
C* K. Brill/NMC		07/91	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* K. Brill/EMC		 6/98	Documentation				*
C* A. Hardy/GSC		 6/98	Cleaned up prolog			*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	CHARACTER*(*)	proj
C*
	CHARACTER	cprj*4
	INTEGER 	isend (3)
	REAL 		rsend (7)
C------------------------------------------------------------------------
C*	Put projection name into an integer.
C
	cprj = proj
	CALL ST_STOI  ( cprj, 4, nv, iprj, ier )
C
C*	Load input parameters into buffer and write them to the mailbox.
C*
	isend (1)  = 10
	isend (2)  = FSMPRJ
	isend (3)  = iprj
	rsend (1)  = angle1
	rsend (2)  = angle2
	rsend (3)  = angle3
	rsend (4)  = dlatll
	rsend (5)  = dlonll
	rsend (6)  = dlatur
	rsend (7 ) = dlonur
C*
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( rsend, 7, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C*
C*	If successful write, get output parameters.
C*
        CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
