	SUBROUTINE G2T_BND ( nz, ns, npts, clat, clon, rlat, rlon, iret)
C************************************************************************
C* G2T_BND								*
C* 									*
C* This subroutine processes a bound area.                              *
C* 									*
C* G2TBND ( NZ, NS, NPTS, CLAT, CLON, RLAT, RLON, IRET )		*
C*									*
C* Input parameters:                                                    *
C*	NZ		INTEGER		Nth zone area			*
C*	NS		INTEGER		Nth subzone			*
C*									*
C* Output parameters:							*
C*	NPTS		INTEGER		No of points in the bound area	*
C*	CLAT		REAL		Centroid lat			*
C*	CLON		REAL		Centroid lon			*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C* 	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-4 = error processing bound file*
C**									*
C* Log:									*
C* T. Lee/SAIC		10/06						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	INCLUDE 	'ERROR.PRM'
	REAL		rlat (*), rlon (*)
	CHARACTER*32	btyp, btag
C-----------------------------------------------------------------------
	iret   = NORMAL 
C
C*	Set the bounds type for searching.
C
	CALL ST_LSTR ( bndtyp, lens, ier )
	btyp = bndtyp ( : lens ) // CHNULL 
	CALL CLO_BSTYPE ( btyp, ier )
	IF  ( ier .ne. NORMAL )  RETURN
	CALL ST_RMBL ( bndtag ( nz,ns ), bndtag ( nz,ns ), lentag, ier )
C
	IF ( lentag .gt. 0 )  THEN
	    btag = bndtag ( nz, ns ) ( : lentag ) // CHNULL 
	    CALL CLO_BSTAG ( btag, ier )
	    IF  ( ier .ne. NORMAL )  THEN
		iret = -4
		RETURN
	    END IF
	END IF
C
C*	Set the geographical limits for searching.
C
	mnpts = 0
	mxpts = LLMXPT
	filter = 0.
C 
	CALL CLO_BGNEXT ( mnpts, mxpts, filter, npts, rlat, rlon, ier)
	IF  ( ier .ne. NORMAL )  THEN
	    iret = -4
	    RETURN
	END IF
C
	IF ( npts .lt. LLMXPT )  THEN
	    npts = npts + 1
	    rlat ( npts ) = rlat ( 1 )
	    rlon ( npts ) = rlon ( 1 )
	END IF
C
	CALL CLO_BGCENT ( clat, clon, ier )
C*
	RETURN
	END
