	    SUBROUTINE MR_INTZ  ( nlev, ipt, stndat, iret )
C************************************************************************
C* MR_INTZ								*
C*									*
C* This subroutine computes heights for significant temperature data	*
C* by interpolating mandatory heights linearly with respect to log p.	*
C* Any heights that cannot be interpolated (i.e., at levels above the	*
C* top mandatory level) are computed using the moist hydrostatic 	*
C* height.								*
C*									*
C* MR_INTZ  ( NLEV, IPT, STNDAT, IRET )				*
C*									*
C* Input parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	IPT    ( NLEV )	INTEGER		Pointers to ordered data	*
C*									*
C* Input and output parameters:						*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/87	Rewritten for GEMPAK4			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		stndat (6,*)
	INTEGER		ipt (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret =  0
	IF  ( nlev .le. 1 )  RETURN
C
C*	Find the top wind which contains height.
C
	maxlev = 0
	ilev   = nlev
	DO WHILE  ( ilev .ge. 1 )
	    IF  ( .not. ERMISS ( stndat ( 6, ipt (ilev) ) ) )  THEN
		maxlev = ilev
		ilev   = 0
	      ELSE
		ilev   = ilev - 1
	    END IF
	END DO
C
C*	Interpolate all levels up to MAXLEV.
C
	pbot = RMISSD
	DO  i = 1, maxlev
C
C*	    Get pressure and height at this level.
C
	    pres = stndat ( 1, ipt ( i ) )
	    hght = stndat ( 6, ipt ( i ) )
C
C*	    Skip data if there is no pressure (only surface).
C
	    IF  ( ERMISS ( pres ) )  THEN
C*		! Ignore.
C
C*		If height is not missing, use this as bottom level.
C
	      ELSE IF  ( .not. ERMISS ( hght ) )  THEN
		pbot = pres
		hbot = hght
		ptop = 2000.0
C
C*		Skip level if no lower bounding level has been found.
C
	      ELSE IF  ( ERMISS ( pbot ) )  THEN
C*		! Ignore level
C
C*		Otherwise, try to interpolate data.
C
	      ELSE
C
C*		Find next level with height.
C
		ilev = i + 1
		DO WHILE  ( pres .le. ptop )
		    IF ( .not. ERMISS ( stndat (6,ipt(ilev) ))) THEN
			ptop = stndat ( 1, ipt (ilev) )
			htop = stndat ( 6, ipt (ilev) )
		      ELSE
			ilev = ilev + 1
		    END IF
		END DO
C
C*		Interpolate data if a level was found.
C
		stndat ( 6, ipt (i) ) = hbot + ( htop - hbot ) *
     +			  ( ALOG ( pres/pbot )  /  ALOG ( ptop/pbot ) )
	    END IF
	END DO
C
C*	Check for missing heights in sounding.  Replace with moist
C*	hydrostatic height.
C
C*	Check if sounding is complete.
C
	IF  ( maxlev .ge. nlev )  RETURN
C
C*	Get variables at MAXLEV.
C
	IF  ( maxlev .gt. 0 )  THEN
	    pb  = stndat ( 1, ipt ( maxlev ) )
	    hb  = stndat ( 6, ipt ( maxlev ) )
	    tb  = stndat ( 2, ipt ( maxlev ) )
	    tdb = stndat ( 3, ipt ( maxlev ) )
	  ELSE
	    pb  = RMISSD
	    hb  = RMISSD
	    tb  = RMISSD
	    tdb = RMISSD
	END IF
C
C*	Loop through remaining levels and compute hydrstatic height.
C
	DO  i = maxlev + 1, nlev
	    IF  ( ERMISS ( stndat ( 6, ipt (i) ) ) )  THEN
		tt  = stndat ( 2, ipt (i) )
		tdt = stndat ( 2, ipt (i) )
		pt  = stndat ( 1, ipt (i) )
		stndat (6, ipt(i) ) = PC_MHGT (tb,tt,tdb,tdt,pb,pt,hb)
	    END IF
C
C*	    Reset bottom level.
C
	    IF  ( ( .not. ERMISS ( stndat ( 2, ipt (i) ) ) ) .and.
     +		  ( .not. ERMISS ( stndat ( 6, ipt (i) ) ) ) )  THEN
		pb  = stndat ( 1, ipt (i) )
		tb  = stndat ( 2, ipt (i) )
		tdb = stndat ( 3, ipt (i) )
		hb  = stndat ( 6, ipt (i) )
	    END IF
	END DO
C*
	RETURN
	END
