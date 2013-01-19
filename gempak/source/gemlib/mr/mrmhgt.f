	SUBROUTINE MR_MHGT  ( nlev, ipt, stndat, sclhgt, iret )
C************************************************************************
C* MR_MHGT								*
C*									*
C* This subroutine computes the moist height for all levels in a	*
C* sounding.  The mandatory heights are replaced by the computed	*
C* heights.								*
C*									*
C* MR_MHGT  ( NLEV, IPT, STNDAT, SCLHGT, IRET )				*
C*									*
C* Input parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	IPT    ( NLEV )	INTEGER		Pointers to ordered data	*
C*									*
C* Input and output parameters:						*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*									*
C* Output parameters:							*
C*	SCLHGT ( NLEV )	REAL		Scale heights			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/86						*
C* M. desJardins/GSFC	 4/89	Account for missing data		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		stndat (6,*), sclhgt (*)
	INTEGER		ipt (*)
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Find first level with pressure, temperature and height.
C
	ilev = 1
	DO WHILE  ( ( ilev .lt. nlev ) .and.
     +		    ( ( ERMISS ( stndat (1, ipt (ilev)) ) ) .or.
     +		      ( ERMISS ( stndat (2, ipt (ilev)) ) ) .or.
     +		      ( ERMISS ( stndat (6, ipt (ilev)) ) ) ) )
	    ilev = ilev + 1
	END DO
C
C*	Get the bottom level.
C
	IF  ( ilev .lt. nlev )  THEN
	    pb  = stndat ( 1, ipt (ilev) )
	    tb  = stndat ( 2, ipt (ilev) )
	    tdb = stndat ( 3, ipt (ilev) )
	    hb  = stndat ( 4, ipt (ilev) )
	END IF
C
C*	Replace the height with the moist hydrostatic height at each 
C*	level.
C
	DO  i = ilev + 1, nlev
	    tt  = stndat ( 2, ipt (i) )
	    tdt = stndat ( 3, ipt (i) )
	    pt  = stndat ( 1, ipt (i) )
	    scale  = PR_SCLH ( tb, tt, tdb, tdt, pb, pt )
	    IF ( .not. ERMISS ( scale ) )  THEN
		sclhgt ( ipt (i) ) = scale
		zzz = PR_MHGT ( hb, pb, pt, scale )
		IF  ( .not. ERMISS ( zzz ) )  THEN
		    stndat ( 6, ipt (ilev) ) = zzz
		    pb  = pt
		    tb  = tt
		    tdb = tdt
		    hb  = zzz
		END IF
	    END IF
	END DO
C*
	RETURN
	END
