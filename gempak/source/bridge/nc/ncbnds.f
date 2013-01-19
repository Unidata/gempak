	SUBROUTINE NC_BNDS ( string, mxp, npt, rlat, rlon, iptr, iret ) 
C************************************************************************
C* NC_BNDS  								*
C*									*
C* This subroutine gets the bounds for a non-convective sigmet report.  *
C*                                                                      *
C* NC_BNDS ( STRING, MXP, NPT, RLAT, RLON, IPTR, IRET )                 *
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Report string                   *
C*	MXP		INTEGER		Maximum number of points        *
C*									*
C* Output parameters:							*
C*	NPT		INTEGER		Number of bounds points         *
C*	RLAT(*)		REAL		Latitudes of bounds points      *
C*	RLON(*)		REAL		Longitudes of bounds points     *
C*	IPTR		INTEGER		Pointer following last bounds pt*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = no bounds given           *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/00	                                        *
C* D. Kidwell/NCEP	 9/00	AM_GTPT -> CLO_DDDEC; fixed iptr bugs   *
C* A. Hardy/NCEP	 8/00   Modified to use br_bnds			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
	REAL		rlat (*), rlon (*)
C*
	CHARACTER	carr (60)*10, strbuf*160, sep*2
C------------------------------------------------------------------------
	iret  = 0
	npt   = 0
	iptr  = 1
        sep   = 'TO'
C
        DO ii = 1, mxp
	    rlat ( ii ) = RMISSD
	    rlon ( ii ) = RMISSD
	END DO 
C
	CALL ST_LSTR ( string, lens, ier )
C
	ifrom = INDEX ( string ( :lens ), 'FROM ' )
	IF ( ifrom .eq. 0 ) THEN
	    iret = -6
	    RETURN
	END IF
C
C*	Get the bounds.
C
        iend = MIN ( ifrom + 159, lens )
        strbuf = string ( ifrom:iend )
	CALL ST_CLST ( strbuf, ' ', ' ', 60, carr, numc, ier )
C
C*	Get the latitude and longitude pairs.
C
        CALL BR_BNDS ( string, mxp, carr, numc, sep, npt, 
     +                 rlat, rlon, iptr, ier )
	iptr = iptr - 1
C*
	RETURN
	END
