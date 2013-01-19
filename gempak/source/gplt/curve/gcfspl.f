	SUBROUTINE GCFSPL  ( xx, yy, np, iend, c, iret )
C************************************************************************
C* GCFSPL								*
C*									*
C* This subroutine gets the coefficients to be use for fitting points	*
C* which are strictly monotonic to a cubic spline.  The coefficients	*
C* are dimensioned (4,np-1)						*
C*									*
C* This algorithm is adapted from "Applied Numerical Analysis", 	*
C* Curtis F. Gerald, pp. 508-509.					*
C*									*
C* GCFSPL ( XX, YY, NP, IEND, C, IRET )					*
C*									*
C* Input parameters:							*
C*	XX (NP)		REAL		Input x coordinates		*
C*	YY (NP)		REAL		Input y coordinates		*
C*	NP		INTEGER		Number of points		*
C*	IEND		INTEGER		End point option		*
C*					 1=linear 2=parabolic 3=cubic	*
C*									*
C* Output parameters:							*
C*	C (4,NP-1)	REAL		Coefficients			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C************************************************************************
	REAL		xx (*), yy (*), c (4, *)
	INCLUDE		'CURVE.CMN'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = 0
C
C*	Call a parabolic curve fitting routine if there are only 3 points.
C
C	IF  ( np .eq. 3 ) THEN
C	    CALL GCFPAR ( xx, yy, c, iret )
C	    RETURN
C	END IF
C
C*	This code is executed when there are at least four points.
C
	iret = NORMAL
	nm2 = np - 2
	nm1 = np - 1
	dx1 = xx (2) - xx (1)
	dy1 = ( yy (2) - yy (1) ) / dx1 * 6.
	DO  i = 1, nm2
	    dx2 = xx (i+2) - xx (i+1)
	    dy2 = ( yy (i+2) - yy (i+1) ) / dx2 * 6.
	    a ( 1, i ) = dx1
	    a ( 2, i ) = 2. * ( dx1 + dx2 )
	    a ( 3, i ) = dx2
	    a ( 4, i ) = dy2 - dy1
	    dx1 = dx2
	    dy1 = dy2
	END DO
C
C*	Adjust the first and last rows to the end condition.
C
	IF  ( iend .eq. 2 ) THEN
	    a ( 2,   1 ) = a ( 2,   1 ) + xx (2) - x (1)
	    a ( 2, nm2 ) = a ( 2, nm2 ) + xx (np) - xx (nm1)
	  ELSE IF ( iend .eq. 3 ) THEN
	    dx1 = xx (2) - xx (1)
	    dx2 = xx (3) - xx (2)
	    a ( 2, 1 ) = ( dx1 + dx2 ) * ( dx1 + 2. * dx2 ) / dx2
	    a ( 3, 1 ) = ( dx2 * dx2 - dx1 * dx1 ) / dx2
	    dxn2 = xx ( nm1 ) - xx ( nm2 )
	    dxn1 = xx ( np ) - xx ( nm1 )
	    a ( 1, nm2 ) = ( dxn2 * dxn2 - dxn1 * dxn1 ) / dxn2
	    a ( 2, nm2 ) = ( dxn1 + dxn2 ) * ( dxn1 + 2. * dxn2 ) / dxn2
	END IF
C
C*	Now solve the tridiagonal system.  First reduce.
C
	DO  i = 2, nm2
	    a (2,i) = a (2,i) - a (1,i) / a (2,i-1) * a (3,i-1)
	    a (4,i) = a (4,i) - a (1,i) / a (2,i-1) * a (4,i-1)
	END DO
C
C*	Now back substitute.
C
	a ( 4,nm2 ) = a ( 4, nm2 ) / a (2,nm2)
	DO  i = 2, nm2
	    j = nm1 - i
	    a (4,j) = ( a (4,j) - a (3,j) * a (4,j+1) ) / a (2,j)
	END DO
C
C*	Now put values into the s array.
C
	DO  i = 1, nm2
	    s (i+1) = a (4,i)
	END DO
C
C*	Get first and last values of s according to end conditions.
C
	IF  ( iend .eq. 1 ) THEN
	    s (1)  = 0.
	    s (np) = 0.
	  ELSE IF ( iend .eq. 2 ) THEN
	    s (1)  = s (2)
	    s (np) = s (nm1)
	  ELSE IF ( iend .eq. 3 ) THEN
	    s (1)  = ( ( dx1 + dx2 ) * s(2) - dx1 * s(3) ) / dx2
	    s (np) = ( ( dxn2 + dxn1 ) * s(nm1) - dxn1 * s(nm2)) / dxn2
	END IF
C
C*	Use the values of S to fill the coefficients array.
C
	DO  i = 1, nm1
	    dx = xx (i+1) - xx (i)
	    c (1,i) = ( s (i+1) - s (i) ) / ( 6. * dx )
	    c (2,i) = s (i) / 2.
	    c (3,i) = ( yy (i+1) - yy (i) ) / dx -
     +			( 2. * dx * s (i) + dx * s (i+1)) / 6.
	    c (4,i) = yy (i)
	END DO
C*
	RETURN
	END
