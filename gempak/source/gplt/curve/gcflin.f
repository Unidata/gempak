	SUBROUTINE GCFLIN  ( x, y, np, coeffs, iret )
C************************************************************************
C* GCFLIN								*
C*									*
C* This subroutine finds the coefficients to fit a set of points with 	*
C* piecewise lines.  The coefficients are is dimensioned as (2,np).	*
C*									*
C* GCFLIN  ( X, Y, NP, COEFFS, IRET )					*
C*									*
C* Input parameters:							*
C*	X (NP)		REAL		X coordinates			*
C*	Y (NP)		REAL		Y coordinates			*
C*	NP		INTEGER		Number of points to plot	*
C*									*
C* Output parameters:							*
C*	COEFFS (2, NP)	REAL		Coefficients for each point	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* I. Graffman/RDS	9/88	Document				*
C************************************************************************
	REAL		x (*), y (*), coeffs (2, *)
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = 0
C
C*	Generate the coefficients.  Note that x(i+1) - x(i) is assumed to
C*	be non-zero.  This is currently only used for strictly monotonic
C* 	functions.
C
	DO  i = 1, np-1
	    coeffs (1, i) = ( y(i+1) - y(i) ) / ( x(i+1) - x(i) )
	    coeffs (2, i) =   y (i) - coeffs (1,i) * x (i)
	END DO
C*
	RETURN
	END
