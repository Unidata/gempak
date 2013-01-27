	SUBROUTINE SNDESP ( xval, np, coeffs, xout, npout, yout, iret )
C************************************************************************
C* SNDESP								*
C*									*
C* This subroutine evaluates points on a cubic curve.			*
C*									*
C* SNDESP ( XVAL, NP, COEFFS, XOUT, NPOUT, YOUT, IRET )			*
C*									*
C* Input parameters:							*
C*	XVAL (NP)	REAL		X input coordinates		*
C*	NP		INTEGER		Number of input points		*
C*	COEFFS (2,NP)	REAL		Coefficients of line segments	*
C*	XOUT (NPOUT)	REAL		X coordinates to evaluate	*
C*	NPOUT		INTEGER		Number of points to evaluate	*
C*									*
C* Output parameters:							*
C*	YOUT (NPOUT)	REAL		Evaluated Y coordinates		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92		Copied from GCESPL		*
C* J. Whistler/SSAI	 4/93		Cleaned up code			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		xval (*), coeffs (4,*), xout (*), yout (*)
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
C*		Do not evaluate if input is out of range.
C
	      ELSE IF ( (xx .lt. xval (1) ) .or. (xx .gt. xval (np)))
     +          THEN
		yout (i) = RMISSD
C
C*		The data to be evaluated is at the first point.
C
	      ELSE IF ( xx .eq. xval (1) ) THEN
		yout (i) = coeffs (4,1) 
C
C*		Find the interval containing the point and evaluate it.
C
	      ELSE 
		j = 2
		DO WHILE ( ( xx .gt. xval (j)) .and. ( j .le. np ) ) 
		    j = j + 1
		END DO
		j = j - 1
		yout (i) = coeffs (1,j) * xx + coeffs (2,j)
		dx = xx - xval (j)
		yout (i) = coeffs (1,j) * dx**3 + coeffs (2,j) * dx**2 +
     +	       		   coeffs (3,j) * dx + coeffs (4,j)
	    END IF
	END DO
C*
	RETURN
	END
