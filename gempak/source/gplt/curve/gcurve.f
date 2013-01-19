	SUBROUTINE GCURVE  ( itype, np, xarray, yarray, npout, xeval,
     +			     yeval, iret )
C************************************************************************
C* GCURVE								*
C* 									*
C* This subroutine fits a curve to a set of input points and then	*
C* evaluates the curve for values of x.  The input points must be	*
C* strictly monotonic in x:						*
C* 									*
C*     xarray (1) < xarray (2) < ... < xarray (np)			*
C*			or						*
C*     xarray (1) > xarray (2) > ... > xarray (np)			*
C* 									*
C* This subroutine simply fits the points to the curve.  It differs	*
C* from GCYEVL in that it does not map the points into the plotting	*
C* coordinate system before the fitting is done.			*
C* 									*
C* GCURVE  ( ITYPE, NP, XARRAY, YARRAY, NPOUT, XEVAL, YEVAL, IRET )	*
C* 									*
C* Input parameters:							*
C*	ITYPE		INTEGER		Type of curve			*
C*				  	  1 = piecewise linear		*
C*				  	  2 = cubic spline		*
C*	NP		INTEGER		Number of input points		*
C* 	XARRAY (NP)	REAL		X input coordinates 		*
C* 	YARRAY (NP)	REAL		Y input coordinates 		*
C*	NPOUT		INTEGER		Number of evaluations		*
C*	XEVAL (NPOUT)	REAL		X evaluation coordinates 	*
C*									*
C* Output parameters:							*
C* 	YEVAL (NPOUT)	REAL		Y evaluated coordinates 	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/90	GEMPAK 5.0				*
C* T. Lee/GSC		 9/97	Fixed typo, NIMONO -> NOMONO		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'CURVE.CMN'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xarray (*), yarray (*), xeval (*), yeval (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for enough points.
C
	IF  ( ( np .lt. 2 ) .or. ( npout .lt. 1 ) ) iret = NOPNTS
	IF  ((np .gt. MAXPTS) .or. (npout .gt. MAXPTS)) iret = NIPNTS
	IF  ( iret .ne. NORMAL ) RETURN
C
C*	Check for type of curve to fit.
C
	CALL GCTYPE ( itype, jtype, jend, ier )
C
C*	Move data into internal arrays sorted in increasing order.
C*	Return with error if data is not strictly monotonic.
C
	delx = xarray (2) - xarray (1)
	IF  ( delx .lt. 0. ) THEN
	    ibegin = np
	    iend   = 1
	    inc    = -1
	  ELSE
	    ibegin = 1
	    iend   = np
	    inc    = 1
	END IF
C*
	x (1) = xarray ( ibegin )
	y (1) = yarray ( ibegin )
	nknt  = 1
	xx    = x (1)
	DO  i = ibegin+inc, iend, inc
	    tx = xarray (i)
	    ty = yarray (i)
	    IF  ( ( .not. ERMISS (tx) ) .and.
     +            ( .not. ERMISS (ty) ) ) THEN
		IF  ( tx .le. xx ) THEN
		    iret = NOMONO
		    RETURN
		  ELSE
		    nknt     = nknt + 1
		    x (nknt) = tx
		    y (nknt) = ty
		    xx       = tx
		END IF
	    END IF
	END DO
C
C*	Check to see if there are still at least two points.
C
	IF  ( nknt .lt. 2 ) THEN
	    iret = NOPNTS
	    RETURN
	END IF
C
C*	Generate coefficients for evaluating curve.
C
	IF  ( jtype .eq. 1 ) THEN
	    CALL GCFLIN ( x, y, nknt, coeffs, iret )
	  ELSE IF ( ( jtype .eq. 2 ) .and. ( nknt .ge. 3 ) ) THEN
	    CALL GCFSPL ( x, y, nknt, jend, coeffs, iret )
	  ELSE
	    DO  i = 1, npout
		yeval (i) = RMISSD
	    END DO
	    RETURN
	END IF
	IF  ( iret .ne. NORMAL ) THEN
	    iret = NORMAL
	    DO  i = 1, npout
		yeval (i) = RMISSD
	    END DO
	    RETURN
	END IF
C
C*	Transform evaluating points to normalized coordinates.
C
	DO  i = 1, npout
	    xout (i) = xeval (i)
	END DO
C
C*	Evaluate curve.
C
	IF  ( jtype .eq. 1 ) THEN
	    CALL GCELIN ( x, nknt, coeffs, xout, npout, yout, iret )
	  ELSE IF  ( jtype .eq. 2 ) THEN
	    CALL GCESPL ( x, nknt, coeffs, xout, npout, yout, iret )
	END IF
C
C*	Move points into output array.
C
	DO  i = 1, npout
	    yeval (i) = yout (i)
	END DO
C*
	RETURN
	END
