	SUBROUTINE  GR_VNAV  ( proj, kx, ky, rlat1, rlon1, rlat2, rlon2,
     +			       angl1, angl2, angl3, angflg, valid, 
     +			       iret )
C************************************************************************
C* GR_VNAV								*
C*									*
C* This subroutineecks the information used to make a navigation block  *
C* for a grid file.  If any of the data fails a validity check, a       *
C* not valid flag is set, and a non -zero return code is passed back.   *
C*									*
C* GR_VNAV  ( PROJ, KX, KY, RLAT1, RLON1, RLAT2, RLON2, ANGL1, 		*
C*            ANGL2, ANGL3, ANGFLG, VALID, IRET )			*
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
C*	VALID 		LOGICAL		validity flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-24 = rotated cylindrical proj	*
C**									*
C* Log:									*
C* S. Gilbert/NCEP	10/06						*
C* H. Edmon/UW		 7/10	Added check on angflg			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	proj
	LOGICAL		valid 
	LOGICAL		angflg
C-----------------------------------------------------------------------
	iret  = 0
        valid = .TRUE.
C
C*	Check for rotated cylindrical projection.
C
        IF ( ( proj .eq. 'CED' ) .or. ( proj .eq. 'MER' ) ) THEN
            IF ( angflg .and.
     +	         ( ( angl3 .lt. -360.0 ) .or.
     + 		   ( angl3 .gt.  360.0 ) ) ) THEN
                valid = .FALSE.
                iret = -24
            ENDIF
        ENDIF
C*
	RETURN
	END
