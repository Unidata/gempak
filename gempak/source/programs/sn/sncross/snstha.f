	SUBROUTINE SNSTHA  ( ithinc, icvtyp, nstn, stns, ipsdat, nlvls,
     +			    stndat, sloc, x, thmin, thmax, pontha, 
     +			    pdat, iret )
C************************************************************************
C* SNSTHA								*
C*									*
C* This subroutine computes a grid of pressure values on theta		*
C* surfaces.								*
C*									*
C* SNSTHA  ( ITHINC, ICVTYP, NSTN, STNS, IPSDAT, NLVLS, STNDAT,		*
C*           SLOC, X, THMIN, THMAX, PONTHA, PDAT, IRET )		*
C*									*
C* Input parameters:							*
C*	ITHINC		INTEGER		Theta interval			*
C*	ICVTYP		INTEGER		Curve type			*
C*	NSTN		INTEGER		Number of stations		*
C*	STNS (NSTN)	CHAR*8		Station ids			*
C*	IPSDAT (NSTN)	INTEGER		Pointers to station data	*
C*	NLVLS  (NSTN)	INTEGER		Number of levels		*
C*	STNDAT (*)	REAL		Station data buffer		*
C*	SLOC   (NSTN)	REAL		Station location on x axis	*
C*	X (LLMAXD)	REAL		Grid locations on x axis	*
C*									*
C* Output parameters:							*
C*	THMIN		REAL		Minimum theta level		*
C*	THMAX		REAL		Maximum theta level		*
C*	PONTHA(LLMAXD,*)REAL		Pressures on isentropic surfaces*
C*	PDAT (LLTMCX,*)	REAL		Pressure on theta surfaces	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = can't find pres. or temp.	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* M. desJardins/GSFC	 2/85	Changed untangling algorithm		*
C* M. desJardins/GSFC	 3/91	Added LLTMCX				*
C* J. Nielsen/TAMU	11/91	Added check for negative pressures	*
C* A. Hardy/GSC		 3/99	Added priority parameter for PC_SSTN    *
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* J. Wu/GSC		 7/00	Swith DATA declaration with ERMISS.FNC	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stns(*)
	INTEGER		ipsdat (*), nlvls (*)
	REAL		stndat (*), sloc (*), x (*), pontha (LLMAXD,*),
     +			pdat (LLTMCX,*)
C*
	LOGICAL		chrflg (2), cmpflg (2), done, start, tangle
	CHARACTER	sfcprm(2)*4, chrdat(2)*8
	REAL		outdat (2), sfctha (LLTMCX),
     +			sxtra (100), ss (LLTMCX), pp (LLTMCX),
     +			dpdt (LLTMCX)
	REAL		pnew (LLMAXD,2), pil(LLMAXD), pilm1 (LLMAXD)
	PARAMETER	( ipcord = 1 )
	PARAMETER	( ivcord = 2 )
	INCLUDE		'ERMISS.FNC'
	DATA		sfcprm / 'PRES', 'THTA' /
C------------------------------------------------------------------------
	iret = 0
	thinc = ithinc
C
C*	Loop through stations getting surface pressure and theta.
C
	npm = 0
	CALL PC_DFLV ( 2, sfcprm, chrflg, cmpflg, npm, ier )
	IF  ( ( .not. cmpflg (1) ) .or. ( .not. cmpflg (2) ) ) THEN
	    iret = -8
	    CALL ER_WMSG  ( 'SNCROSS', iret, ' ', ier )
	    RETURN
	END IF
C
	DO  i = 1, nstn
            ispri = 0
	    CALL PC_SSTN ( stns (i), 0, 0., 0., 0., ispri, IMISSD, 
     +		           nlvls (i), ier )
	    CALL PC_CMLV ( 1, stndat (ipsdat(i)), outdat, chrdat, ier )
	    pdat ( i, 1 ) = outdat (1)
	    sfctha (i) = outdat (2)
	    IF ( ERMISS ( sfctha (i) ) .or. ERMISS ( pdat ( i,1 ) ) )
     +		THEN
		iret = -19
		CALL ER_WMSG ( 'SNCROSS', iret, ' ', ier )
		RETURN
	    END IF
C
C*	    Find theta at 100 mb above surface and compute delta p / 
C*	    delta theta to use later in extrapolating data below surface.
C
	    p100 = pdat ( i,1 ) - 100.
	    CALL PC_CMVR ( p100, ipcord, stndat (ipsdat(i) ), outdat,
     +			    chrdat, ier )
	    th100 = outdat (2)
	    dpdt (i) = 100. / ( sfctha (i) - th100 )
	    IF  ( dpdt (i) .gt. -4. ) dpdt (i) = -4.
	    IF  ( dpdt (i) .lt. -3. ) dpdt (i) = -30.
	END DO
C
C*	Get minimum value of theta making sure it lies on a theta
C*	increment.
C
	thmin = sfctha (1)
	DO  i = 2, nstn
	    IF  ( sfctha (i) .lt. thmin ) thmin = sfctha (i)
	END DO
	ithmin = thmin / thinc
	thmin = FLOAT ( ithmin * ithinc )
C
C*	Get pressure data on the isentropic surfaces and find the
C*	maximum value for theta.
C
	CALL PC_DFLV ( 1, sfcprm, chrflg, cmpflg, npm, ier )
	DO  i = 1, nstn
C
C*	    Set station.  Only nlvls will be used.
C
            ispri = 0
	    CALL PC_SSTN ( stns (i), 0, 0., 0., 0., ispri, IMISSD, 
     +			   nlvls (i), ier )
	    done = .false.
	    start = .false.
	    vlev = thmin
	    j    = 2
	    DO  k = 2, 200
		pdat (i,k) = RMISSD
	    END DO
	    DO WHILE ( .not. done )
C
C*		Get data for next level.
C
		CALL PC_CMVR ( vlev, ivcord, stndat (ipsdat(i)), pres,
     +				chrdat, ier )
		IF   ( .not. ERMISS (pres ))  THEN
		    pdat (i,j) = pres
		    j = j + 1
		    vlev = vlev + thinc
		    start = .true.
		    IF  ( j .gt. 200 ) done = .true.
		  ELSE IF  ( .not. start )  THEN
		    pdat (i,j) = pdat (i,1) - dpdt (i) * ( sfctha (i) - 
     +					vlev )
		    j = j + 1
		    vlev = vlev + thinc
		    IF  ( j .gt. 200 ) done = .true.
		  ELSE
		    done = .true.
		END IF
		IF  ( done )  THEN
		    IF  ( i .eq. 1 ) THEN
			thmax = vlev - thinc
			maxl  = j - 1
		      ELSE
			IF  ( thmax .lt. (vlev-thinc) ) THEN
			    thmax = vlev-thinc
			    maxl  = j - 1
			END IF
		    END IF
		END IF
	    END DO
	END DO
C
C*	Generate grid at LLMAXD evenly spaced points.
C
	DO  il = 1, maxl
	    ilm1 = il - 1
C
C*	    Find the station values to use.
C
	    CALL SNSPTS ( il, nstn, pdat, sloc, ns, ss, pp, ier )
C
C*	    Evaluate the points along the curve.
C
	    CALL GCYEVL ( icvtyp, ns, ss, pp, LLMAXD, x,
     +			  pontha (1,il), ier )
C
	    DO ix = 1, LLMAXD
		IF ( pontha(ix,il) .lt. 0. )  pontha(ix,il) = RMISSD
	    END DO
C
C*	    Check the data for crossings.
C
	    CALL SNSTAN ( il, thinc, x, pontha, nxtra, sxtra, 
     +			  pil, pilm1, tanmax, ier )
C
C*	    Recompute upper and lower curves if necessary.
C
	    IF  ( nxtra .ne. 0 )  THEN
C*
	      CALL SNSREC ( il,   nstn, pdat, sloc, nxtra, sxtra,
     +                       pil, x, icvtyp, pnew (1,2), ier )
	      CALL SNSREC ( ilm1, nstn, pdat, sloc, nxtra, sxtra,
     +                      pilm1, x, icvtyp, pnew (1,1), ier )
C
C*	      Check to see if new curves are tangled.
C
	      CALL SNSTAN ( 2, thinc, x, pnew, nxtra, sxtra, pil, 
     +			    pilm1, tmax, ier )
C
C*	      If tangling was reduced, save new values.
C
	      IF  (tmax .gt. .6*tanmax) THEN
		DO  i = 1, LLMAXD
		    IF ( pnew (i,2) .lt. 0. ) pnew (i,2) = RMISSD
		    IF ( pnew (i,1) .lt. 0. ) pnew (i,1) = RMISSD
		    pontha (i,il)   = pnew (i,2)
		    pontha (i,ilm1) = pnew (i,1)
		END DO

C
C*		Untangle lower levels if necessary.
C
		done = .false.
		ilev = il - 1
		DO WHILE ( (.not. done) .and. (ilev .gt. 2 ) )
		    ilevm1 = ilev - 1
C
C*		    Check the data at lower level for crossings.
C
		    CALL SNSTAN ( ilev, thinc, x, pontha, nxtra, 
     +				  sxtra, pil, pilm1, tanmax, ier )
C
C*		    Recompute curves if tangled.
C
		    IF  ( nxtra .eq. 0 ) THEN
			done = .true.
		      ELSE
			CALL SNSREC (ilev, nstn, pdat, sloc, nxtra,
     +                               sxtra,
     +				     pil, x, icvtyp, pnew (1,2), ier )
			CALL SNSREC (ilevm1, nstn, pdat, sloc, nxtra,
     +                               sxtra,
     +				     pilm1, x, icvtyp, pnew (1,1), ier )
C
C*			See if new curves are tangled.  If not, use
C*			new values.
C
			CALL SNSTAN ( 2, thinc, x, pnew, nxtra, sxtra,
     +				      pil, pilm1, tmax, ier )
			IF  ( tmax .lt. .6 * tanmax ) THEN
			    IF  ( nxtra .ne. 0 ) tangle = .true.
			    DO  i = 1, LLMAXD
		    		IF (pnew(i,2).lt.0.) pnew(i,2)=RMISSD
		    		IF (pnew(i,1).lt.0.) pnew(i,1)=RMISSD
				pontha (i,ilev)   = pnew (i,2)
				pontha (i,ilevm1) = pnew (i,1)
			    END DO
			  ELSE
			    done = .true.
			END IF
		    END IF
		    ilev = ilev - 1
		END DO
	      END IF
	    END IF
	END DO
C
C*	Untangle any isentropes still tangled.
C
	CALL SNSCKD ( maxl, pontha, x, ier )
C
C*	Make sure we didn't go overboard.
C
	DO  il = 1, maxl
	  DO  i = 1, LLMAXD
	    IF  ( pontha (i,il) .le. 0. )  pontha (i,il) = RMISSD
	  END DO  
	END DO
C*
	RETURN
	END
