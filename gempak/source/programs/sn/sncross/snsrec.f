	SUBROUTINE SNSREC ( ilev, nstn, pdat, sloc, nxtra, sval, pval,
     +			    x, icvtyp, pnew, iret )
C************************************************************************
C* SNSREC								*
C*									*
C* This subroutine recomputes an isentrope using station points and	*
C* additional points where the curve was tangled.			*
C*									*
C* SNSREC ( ILEV, NSTN, PDAT, SLOC, NXTRA, SVAL, PVAL, X, ICVTYP, PNEW,	*
C*          IRET )							*
C*									*
C* Input parameters:							*
C*	ILEV		INTEGER		Level number			*
C*	NSTN		INTEGER		Number of stations		*
C*	PDAT (NSTN)	REAL		Pressure data at stations	*
C*	SLOC (NSTN)	REAL		Location of stations		*
C*	NXTRA		INTEGER		Number of extra points		*
C*	SVAL (NXTRA)	REAL		Location of extra points	*
C*	PVAL (NXTRA)	REAL		Pressure at extra points	*
C*	X (LLMAXD)	REAL		Location of grid points		*
C*	ICVTYP		INTEGER		Curve type			*
C*									*
C* Output parameters:							*
C*	PNEW (LLMAXD)	REAL		Pressure on isentrope		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C************************************************************************
	REAL		pdat (*), sloc (*), sval (*), pval (*), x (*),
     +			pnew (*)
	REAL		pp (60), ss (60)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the pressure values for this level at the stations.
C
	CALL SNSPTS ( ilev, nstn, pdat, sloc, ns, ss, pp, ier )
C
C*	Merge the stations with the extra points.
C
	DO  i = 1, nxtra
	    ns = ns + 1
	    ss (ns) = sval (i)
	    pp (ns) = pval (i)
	END DO
C
C*	Sort the stations before fitting the curve.
C
	DO  i = 1, ns-1
	    DO  j = i+1, ns
		IF  ( ss (j) .lt. ss (i) ) THEN
		    ssav = ss (j)
		    psav = pp (j)
		    ss (j) = ss (i)
		    pp (j) = pp (i)
		    ss (i) = ssav
		    pp (i) = psav
		END IF
	    END DO
	END DO
C
C*	Evaluate the curve.
C
	CALL GCYEVL ( icvtyp, ns, ss, pp, LLMAXD, x, pnew, ier )
C*
	RETURN
	END 
