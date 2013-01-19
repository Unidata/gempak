	SUBROUTINE MR_SCMZ  ( nlev, ipt, stndat, sclhgt, iret )
C************************************************************************
C* MR_SCMZ								*
C*									*
C* This subroutine computes the height to be assigned to significant	*
C* temperature levels using a moist hydrostatic computation.  The	*
C* scale heights used to compute the heights are scaled to leave the	*
C* reported mandatory level heights.					*
C*									*
C* MR_SCMZ  ( NLEV, IPT, STNDAT, SCLHGT, IRET )				*
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
C* M. desJardins/GSFC	 4/89	Added checks for missing data		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		stndat (6,*), sclhgt (*)
	INTEGER		ipt (*)
	LOGICAL		top, found, mand
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	ilev = 0
	top  = .false.
C
C*	Find first level with both pressure and height data.
C
	found = .false.
	DO WHILE ( ( .not. found ) .and. ( .not. top ) )
	    ilev = ilev + 1
	    IF ( ilev .gt. nlev ) THEN
		top = .true.
	      ELSE IF ( ( .not. ERMISS ( stndat ( 1, ipt (ilev) ) ) )
     +		  .and. ( .not. ERMISS ( stndat ( 2, ipt (ilev) ) ) )
     +		  .and. ( .not. ERMISS ( stndat ( 6, ipt (ilev) ) ) ) )
     +								THEN
		found = .true.
	    END IF
	END DO
C
C*	Loop through the rest of the levels.
C
	DO WHILE  ( .not. top )
	    pb    = stndat ( 1, ipt (ilev) )
	    hb    = stndat ( 6, ipt (ilev) )
	    tb    = stndat ( 2, ipt (ilev) )
	    tdb   = stndat ( 3, ipt (ilev) )
	    plev  = stndat ( 1, ipt (ilev) )
	    zlev  = stndat ( 6, ipt (ilev) )
	    jlev = ilev 
	    klev = 0
	    mand = .false.
C
C*	    Find next level with height information.
C
	    DO WHILE ( .not. mand )
		jlev = jlev + 1
		IF  ( jlev .gt. nlev ) THEN
		    mand = .true.
		    top  = .true.
		  ELSE
		    pres = stndat (1, ipt (jlev) )
		    temp = stndat (2, ipt (jlev) )
		    dwpt = stndat (3, ipt (jlev) )
		    hght = stndat (6, ipt (jlev) )
		    IF  ( ( .not. ERMISS ( hght ) ) .and. 
     +			  ( .not. ERMISS ( temp ) ) )  THEN
			mand = .true.
			klev = jlev
		    END IF
C
C*		    Compute scale height to this level.
C
		    scale = PR_SCLH ( tb, temp, tdb, dwpt, pb, pres )
		    sclhgt ( ipt ( jlev ) ) = scale
C
C*		    Compute new height, but don't save it yet.
C
		    znew = PR_MHGT ( hb, pb, pres, scale )
C
C*		    Reassign variables.
C
		    IF  ( .not. ERMISS (znew) )  THEN
			tb  = temp
			tdb = dwpt
			pb  = pres
			hb  = znew
		    END IF
		END IF
	    END DO
C
C*	    If another level with height was found, recompute scale
C*	    heights to be consistent with given mandatory heights.
C
	    IF ( klev .ne. 0 ) THEN
		s = ( hght - zlev ) / ( znew - zlev )
		DO  i = ilev + 1, klev
		    sclhgt ( ipt (i) ) = sclhgt ( ipt (i) ) * s
		END DO
	    END IF
C
C*	    Compute new heights.
C
	    hbb = zlev
	    pbb = plev
	    DO  i = ilev + 1, jlev - 1
		pt    = stndat ( 1, ipt (i) )
		scale = sclhgt ( ipt (i) )
		z     = PR_MHGT ( hbb, pbb, pt, scale )
		stndat ( 6, ipt (i) ) = z
		hbb   = z
		pbb   = pt
	    END DO
C
C*	    Reset pointer to last level with height.
C
	    ilev = klev
	END DO
C*
	RETURN
	END
