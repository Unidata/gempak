	SUBROUTINE  GR_MNAV  ( proj, kx, ky, rlat1, rlon1, rlat2, rlon2,
     +			       angl1, angl2, angl3, angflg, rnvblk, 
     +			       iret )
C************************************************************************
C* GR_MNAV								*
C*									*
C* This subroutine makes a navigation block for a grid file.  The 	*
C* projection may be any simple, full or graph projection.  If 		*
C* ANGFLG is set, the projection must be a full map projection.  	*
C* Otherwise, a simple map projection will be defined.			*
C*									*
C* GR_MNAV  ( PROJ, KX, KY, RLAT1, RLON1, RLAT2, RLON2, ANGL1, 		*
C*            ANGL2, ANGL3, ANGFLG, RNVBLK, IRET )			*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*		Projection name			*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points 	*
C*	RLAT1		REAL		Lower left latitude/x		*
C*	RLON1		REAL		Lower left longitude/y		*
C*	RLAT2		REAL		Upper right latitude/x		*
C*	RLON2		REAL		Upper right longitude/y		*
C*	ANGL1		REAL		Projection angle 1		*
C*	ANGL2		REAL		Projection angle 2		*
C*	ANGL3		REAL		Projection angle 3		*
C*	ANGFLG		LOGICAL		Full projection flag		*
C*									*
C* Output parameters:							*
C*	RNVBLK (LLNNAV)	REAL		Navigation block		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/88	Rewrote for GEMPAK 4			*
C* K. Brill/NMC		01/92	Initialize the navigation block to zero *
C* K. Brill/NMC		02/92	Use LLNNAV				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	proj
	REAL		rnvblk (*)
	LOGICAL		angflg
C*
	CHARACTER	cproj*4
C-----------------------------------------------------------------------
	iret  = 0
C
C*	Initialize the navigation block to zero.
C
	DO i = 1, LLNNAV
	    rnvblk (i) = 0.0
	END DO
C
C*	Encode projection name in word two.
C
	cproj = proj
	CALL ST_CTOI  ( cproj, 1, rnvblk (2), ier )
C
C*	Fill rest of navigation block.
C
	rnvblk (3)  = 1
	rnvblk (4)  = 1
	rnvblk (5)  = kx
	rnvblk (6)  = ky
	rnvblk (7)  = rlat1
	rnvblk (8)  = rlon1
	rnvblk (9)  = rlat2
	rnvblk (10) = rlon2
	rnvblk (11) = angl1
	rnvblk (12) = angl2
	rnvblk (13) = angl3
C
C*	Set nav for graph projections.
C
	IF  ( ( proj .eq. 'LIN' ) .or. ( proj .eq. 'LOG' ) .or.
     +	      ( proj .eq. 'KAP' ) .or. ( proj .eq. 'POL' ) )  THEN
	    rnvblk (1) = 3.0
	  ELSE IF  ( angflg )  THEN
	    rnvblk (1) = 2.0
	  ELSE
	    rnvblk (1) = 1.0
	END IF
C*
	RETURN
	END
