	SUBROUTINE SNSPRM  ( parm, nstn, stns, ipsdat, nlvls, stndat, 
     +			     sloc, x, p, vcord, ivcord, icvtyp, grid, 
     +			     pontha, iret )
C************************************************************************
C* SNSPRM								*
C*									*
C* This subroutine computes LLMAXD by LLMAXD grid of data for the	*
C* parameter specified.							*
C*									*
C* SNSPRM  ( PARM, NSTN, STNS, IPSDAT, NLVLS, STNDAT, SLOC, X, P,	*
C*           VCORD, IVCORD, ICVTYP, GRID, PONTHA, IRET )		*
C*									*
C* Input parameters:							*
C*	PARM		CHAR*		Parameter name			*
C*	NSTN		INTEGER		Number of stations		*
C*	STNS (NSTN)	CHAR*		Station ids			*
C*	IPSDAT (NSTN)	INTEGER		Pointers to station data	*
C*	NLVLS  (NSTN)	INTEGER		Number of levels		*
C*	STNDAT (*)	REAL		Station data buffer		*
C*	SLOC   (NSTN)	REAL		Station location on x axis	*
C*	X (LLMAXD)	REAL		Grid locations on x axis	*
C*	P (LLMAXD)	REAL		Pressures on y axis		*
C*	VCORD		CHAR*		Vertical coordinate		*
C*	IVCORD		INTEGER		Numeric vertical coordinate	*
C*	ICVTYP		INTEGER		Curve type			*
C*									*
C* Output parameters:							*
C*	GRID		REAL		Gridded parameter		*
C*	(LLMAXD,LLMAXD)							*
C*	PONTHA (LLMAXD)	REAL		Vertical coordinate at sfc	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-13 = parm is not computable	*
C*					-14 = parm is char type		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/90						*
C* J. Whistler/SSAI	 2/91	Added ICVTYP to the call		*
C* M. desJardins/GSFC	 3/91	Eliminate error msg when snparm is blank*
C* K. Brill/NMC		02/92	If PARM is ' ', generate sfc only; add	*
C*				parm conditions				*
C* A. Hardy/GSC		 3/99   Added priority parameter to PC_SSTN     * 
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stns (*), parm, vcord
	INTEGER		ipsdat (*), nlvls (*)
	REAL		stndat (*), sloc (*), x (*), p (*), 
     +			grid (LLMAXD, LLMAXD), pontha (*)
C*
	LOGICAL		chrflg (2), cmpflg (2)
	CHARACTER	sparms (2)*4, chrdat (2)*8
	REAL		outdat (2), pp (100), ss (100), pdat (100),
     +			sdat (LLTMCX,LLMAXD), crvout (LLMAXD)
	CHARACTER	tmprm (MMPARM)*4, tmcnd (MMPARM)*12,
     +			prmcnd (2)*12
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
C
C*	Set up PC package to compute vertical coordinate at surface 
C*	and the parameter at requested levels.
C
C*	Pull out paramter name and conditions.
C
	CALL IN_PRMC ( 1, parm, tmprm, tmcnd, np, ier )
	sparms (1) =  vcord
	sparms (2) =  tmprm (1)
	prmcnd (1) = ' '
	prmcnd (2) = tmcnd (1)
	CALL PC_DFLV  ( 2, sparms, chrflg, cmpflg, npm, ier )
	CALL PC_SLCD  ( 2, prmcnd, ier )
	istop = 2
	IF ( parm .eq. ' ' ) istop = 1
	DO  i = 1, istop
	    IF  ( .not. cmpflg (i) )  THEN
		iret = -13
		IF  ( parm .ne. ' ' )  THEN
		    CALL ER_WMSG  ( 'SNCROSS', iret, sparms (i), ier )
		END IF
	      ELSE IF  ( chrflg (i) )  THEN
		iret = -14
		CALL ER_WMSG  ( 'SNCROSS', iret, sparms (i), ier )
	    END IF
	END DO
	IF  ( iret .ne. 0 )  RETURN
C
C*	Loop through all the stations computing data.
C
	DO  i = 1, nstn
C
C*	    Set the station and get the value of pressure at the
C*	    surface.
C
            ispri = 0
	    CALL PC_SSTN  ( stns (i), 0, 0., 0., 0., ispri, IMISSD, 
     +			    nlvls (i), ier )
	    CALL PC_CMLV  ( 1, stndat (ipsdat(i)), outdat, chrdat, 
     +			    ier )
	    pdat ( i ) = outdat (1)
C
C*	    For each pressure in the grid, get the data.
C
	    IF ( istop .eq. 2 ) THEN
	      DO  j = 1, LLMAXD
		CALL PC_CMVR  ( p (j), ivcord, stndat ( ipsdat (i) ), 
     +				outdat, chrdat, ier )
		sdat (i,j) = outdat (2)
	      END DO
	    END IF
	END DO
C
C*	Get values of pressure at the LLMAXD grid points.
C
	mstn = 0
	DO  i = 1, nstn
	    IF  ( .not. ERMISS ( pdat (i) ) )  THEN
		mstn = mstn + 1
		pp ( mstn ) = pdat ( i )
		ss ( mstn ) = sloc ( i )
	    END IF
	END DO
C
C*	Evaluate the curve.
C
	IF  ( mstn .lt. 4 )  THEN
	    DO  k = 1, LLMAXD 
		pontha ( k ) = RMISSD
	    END DO
	  ELSE
	    CALL GCURVE  ( icvtyp, mstn, ss, pp,
     +			   LLMAXD, x, pontha, ier )
	END IF
C
C*	Initialize output grid.
C
	IF ( istop .eq. 2 ) THEN
	  DO  i = 1, LLMAXD
	    DO  j = 1, LLMAXD
		grid (i,j) = RMISSD
	    END DO
	  END DO
C
C*	  Generate grid at LLMAXD evenly spaced points.
C
	  DO  j = 1, LLMAXD
C
C*	    Find the station values to use, eliminating missing
C*	    data.
C
	    mstn = 0
	    DO  i = 1, nstn
		IF  ( .not. ERMISS ( sdat ( i, j ) ) )  THEN
		    mstn = mstn + 1
		    pp ( mstn ) = sdat ( i, j )
		    ss ( mstn ) = sloc ( i )
		END IF
	    END DO
C
C*	    Evaluate the curve.  
C
	    IF  ( mstn .ge. 4 )  THEN
		CALL GCURVE  ( icvtyp, mstn, ss, pp, LLMAXD, x,
     +			       crvout, ier )
		IF  ( ier .eq. 0 )  THEN
		    DO  i = 1, LLMAXD 
			grid ( i, j ) = crvout (i)
		    END DO
		END IF
	    END IF
	  END DO
	END IF
C*
	RETURN
	END
                                    
