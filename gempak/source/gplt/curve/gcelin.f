	SUBROUTINE GCELIN ( xval, np, coeffs, xout, npout, yout, iret )
C************************************************************************
C* GCELIN								*
C*									*
C* This subroutine evalutes points on a piecewise linear curve.  The	*
C* coefficients to be used were found in GCFLIN.			*
C*									*
C* GCELIN ( XVAL, NP, COEFFS, XOUT, NPOUT, YOUT, IRET )			*
C*									*
C* Input parameters:							*
C*	XVAL (NP)	REAL		X input coordinates		*
C*	NP		INTEGER		Number of fitted points 	*
C*	COEFFS (2,NP)	REAL		Coefficients of line segments	*
C*	XOUT (NPOUT)	REAL		X coordinates to evaluate	*
C*	NPOUT		INTEGER		Number of points to evaluate	*
C*									*
C* Output parameters:							*
C*	YOUT (NPOUT)	REAL		Evaluated Y coordinates 	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		xval (*), coeffs (2,*), xout (*), yout (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through all the points.
C
	DO  i = 1, npout
	    xx = xout (i)
C
C*		Do not evaluate if input is "missing".
C
	    IF ( ERMISS (xx)) THEN
		yout (i) = RMISSD
C
C*		Do not evaluate if input is out of bounds.
C
	      ELSE IF ( (xx .lt. xval (1) ) .or. (xx .gt. xval (np)))
     +          THEN
		yout (i) = RMISSD
C
C*		Check for input at first point.
C
	      ELSE IF ( xx .eq. xval (1) ) THEN
		yout (i) = coeffs (1,1) * xx + coeffs (2,1)
C
C*		Find the interval containing the point and use the
C*		coefficients to evaluate.
C
	      ELSE 
		j = 2
		DO WHILE ( ( xx .gt. xval (j)) .and. ( j .le. np ) ) 
		    j = j + 1
		END DO
		j = j - 1
		yout (i) = coeffs (1,j) * xx + coeffs (2,j)
	    END IF
	END DO 
C*
	RETURN
	END
