	SUBROUTINE DMATRX ( itype, np, xpt, ypt, dens, amtrx, coeff,
     +			    chord, npt, nout, iret )
C************************************************************************
C* DMATRX								*
C*									*
C* This subroutine computes the values for the chord length of each	*
C* segment, the coefficient matrix and a transformation matrix used	*
C* in the Gaussian matrix elimination.					*
C*									*
C* DMATRX ( ITYPE, NP, XPT, YPT, DENS, AMTRX, COEFF,			*
C*	    CHORD, NPT, NOUT, IRET )					*
C*									*
C* Input parameters:							*
C*	ITYPE		INTEGER		Type of end points		*
C*					  0 = relaxed			*
C*					  1 = cyclic			*
C*	NP		INTEGER		Number of input points		*
C*	XPT (NP)	REAL		X input coordinates		*
C*	YPT (NP)	REAL		Y input coordinates		*
C*	DENS		REAL		Density of intermediate points	*
C*									*
C* Output parameters:							*
C*	AMTRX (LLMXPT,3)  REAL		Transformation matrix		*
C*	COEFF (LLMXPT,2)  REAL		Coefficient matrix		*
C*	CHORD (NP)	  REAL		Chord lengths			*
C*	NPT (NP)	  INTEGER	Number of points per segment	*
C*	NOUT		  INTEGER	Total number of output points	*
C*	IRET		  INTEGER	Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/98						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	INTEGER		npt (*)
	REAL		xpt (*), ypt (*), amtrx (LLMXPT,3),
     +			coeff (LLMXPT,2), chord (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Compute the chord length of each segment, and the total length.
C
	DO  i = 1, np-1
	    q1 = xpt(i+1) - xpt(i)
	    q1 = q1 ** 2
	    q2 = ypt(i+1) - ypt(i)
	    q2 = q2 ** 2
	    chord(i) = SQRT (q1 + q2)
	END DO
C
C*	Compute the number of additional points for each segment.
C
	nout = 0
	DO  i = 1, np-1
	    npt(i) = NINT ( chord(i) / ( crvscl / dens ) )
	    nout = nout + npt(i)
	END DO
	nout = nout + np
	IF  ( nout .gt. LLMXPT )  THEN
	    iret = NIPNTS
	    RETURN
	END IF
C
C*	Compute the transformation matrix values for the
C*	cyclic conditions.
C
	IF  ( itype .eq. 2 )  THEN
	    amtrx(1,3)  = chord(np-1) / chord(1)
	    amtrx(1,2)  = 2.0 * (1.0 + amtrx(1,3))
	  ELSE
C
C*	    Set the transformation matrix values for the
C*	    relaxed conditions.
C
	    amtrx(1,2)  = 1.0
	    amtrx(1,3)  = 0.5
	    amtrx(np,1) = 2.0
	    amtrx(np,2) = 4.0
	END IF
C
C*	Fill in the body of the coefficient matrix.
C
	DO  i = 2, np-1
	    q1 = 3.0 / ( chord(i-1) * chord(i) )
	    q2 =      ( chord(i-1)**2 ) * ( xpt(i+1) - xpt(  i) )
	    q2 = q2 + ( chord(  i)**2 ) * ( xpt(  i) - xpt(i-1) )
	    coeff(i,1) = q2 * q1
	    q2 =      ( chord(i-1)**2 ) * ( ypt(i+1) - ypt(  i) )
	    q2 = q2 + ( chord(  i)**2 ) * ( ypt(  i) - ypt(i-1) )
	    coeff(i,2) = q2 * q1
	END DO
C
C*	Compute the coefficient matrix values for the cyclic conditions.
C
	IF  ( itype .eq. 2 )  THEN
	    a1 = chord(np-1) / ( chord(1) * chord(1) )
	    q1 = ( xpt( 2) - xpt(   1) ) * a1
	    q2 = ( xpt(np) - xpt(np-1) ) / chord(np-1)
	    coeff(1,1) = 3.0 * ( q1 + q2 )
	    q1 = ( ypt( 2) - ypt(   1) ) * a1
	    q2 = ( ypt(np) - ypt(np-1) ) / chord(np-1)
	    coeff(1,2) = 3.0 * ( q1 + q2 )
	  ELSE
C
C*	    Compute the coefficient matrix values for the
C*	    relaxed conditions.
C
	    coeff( 1,1) = ( 1.5 * ( xpt( 2)-xpt(   1) ) ) / chord(1)
	    coeff( 1,2) = ( 1.5 * ( ypt( 2)-ypt(   1) ) ) / chord(1)
	    coeff(np,1) = ( 6.0 * ( xpt(np)-xpt(np-1) ) ) / chord(np-1)
	    coeff(np,2) = ( 6.0 * ( ypt(np)-ypt(np-1) ) ) / chord(np-1)
	END IF
C
C*	Fill in the rest of the transformation matrix.
C
	DO  j = 2, np-1
	    amtrx(j,1) = chord(j)
	    amtrx(j,2) = 2 * ( chord(j) + chord(j-1) )
	    amtrx(j,3) = chord(j-1)
	END DO
C*
	RETURN
	END
