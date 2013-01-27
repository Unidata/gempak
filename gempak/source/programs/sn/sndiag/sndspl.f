	SUBROUTINE SNDSPL ( nparms, np, nlvin, datain,
     +			    nlvout, hghts, dataout, iret )
C************************************************************************
C* SNDSPL								*
C* 									*
C* This subroutine fits a curve to a set of input points and then	*
C* evaluates the curve for values of x.  The input points must be	*
C* strictly monotonic in x:						*
C* 									*
C*     datain (1) < datain (2) < ... < datain (nlvin)			*
C*			or						*
C*     datain (1) > datain (2) > ... > datain (nlvin)			*
C* 									*
C* SNDSPL ( NPARMS, NP, NLVIN, DATAIN, NLVOUT, HGHTS, DATAOUT, IRET )	*
C* 									*
C* Input parameters:							*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NP		INTEGER		Current parameter		*
C*	NLVIN		INTEGER		Number of levels in		*
C*	DATAIN (LLMXLV) REAL		Input sounding data		*
C*	NLVOUT		INTEGER		Number of levels out		*
C*	HGHTS (NLVOUT) 	REAL		Height levels			*
C* 									*
C* Output parameters:							*
C*	DATAOUT(LLMXLV)	REAL		Output sounding data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92		Copied from GCURVE		*
C* J. Whistler/SSAI	 4/93		Cleaned up code			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'CURVE.CMN'
C*
	REAL		datain(*), dataout(*), hghts(*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for enough points.
C
	IF  ( ( nlvin .lt. 3 ) .or. ( nlvout .lt. 1 ) )
     +		iret = NOPNTS
	IF  ((nlvin .gt. MAXPTS) .or. (nlvout .gt. MAXPTS))
     +		iret = NIPNTS
	IF  ( iret .ne. NORMAL ) RETURN
C
C*	Move data into internal arrays sorted in increasing order.
C*	Return with error if data is not strictly monotonic.
C
	delx = datain(1*nparms+1) - datain(0*nparms+1)
	IF  ( delx .lt. 0. ) THEN
	    ibegin = nlvin
	    iend   = 1
	    inc    = -1
	ELSE
	    ibegin = 1
	    iend   = nlvin
	    inc    = 1
	END IF
C*
	x(1) = datain( (ibegin-1)*nparms +  1 )
	y(1) = datain( (ibegin-1)*nparms + np )
	nknt  = 1
	xx    = x (1)
	DO  i = ibegin+inc, iend, inc
	    tx = datain( (i-1)*nparms +  1 )
	    ty = datain( (i-1)*nparms + np )
	    IF  ( ( .not. ERMISS (tx) ) .and.
     +            ( .not. ERMISS (ty) ) ) THEN
		IF  ( tx .le. xx ) THEN
		    iret = NIMONO
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
	jend = 1
	CALL SNDFSP ( x, y, nknt, jend, coeffs, iret )
C
	IF  ( iret .ne. NORMAL ) THEN
	    iret = NORMAL
	    DO  i = 1, nlvout
		dataout( (i-1)*nparms + np ) = RMISSD
	    END DO
	    RETURN
	END IF
C
C*	Transform evaluating points to normalized coordinates.
C
	DO  i = 1, nlvout
	    xout(i) = hghts(i)
	END DO
C
C*	Evaluate curve.
C
	CALL SNDESP ( x, nknt, coeffs, xout, nlvout, yout, iret )
C
C*	Move points into output array.
C
	DO  i = 1, nlvout
	    dataout( (i-1)*nparms + np ) = yout(i)
	END DO
C*
	RETURN
	END
