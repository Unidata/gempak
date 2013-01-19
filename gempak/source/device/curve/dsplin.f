	SUBROUTINE DSPLIN ( itype, np, xpt, ypt, dens,
     +			    nout, xcv, ycv, iret )
C************************************************************************
C* DSPLIN								*
C*									*
C* This subroutine performs a cubic spline fit to NP data points using	*
C* relaxed boundary conditions.						*
C*									*
C* DSPLIN ( ITYPE, NP, XPT, YPT, DENS, NOUT, XCV, YCV, IRET )		*
C*									*
C* Input parameters:							*
C*	ITYPE		INTEGER		Type of end points		*
C*					  1 = relaxed			*
C*					  2 = cyclic			*
C*	NP		INTEGER		Number of input points		*
C*	XPT (NP)	REAL		X input coordinates		*
C*	YPT (NP)	REAL		Y input coordinates		*
C*	DENS		REAL		Density of intermediate points	*
C*									*
C* Output parameters:							*
C*	NOUT		INTEGER		Number of output points		*
C*	XCV (NOUT)	REAL		X evaluated coordinates		*
C*	YCV (NOUT)	REAL		Y evaluated coordinates		*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* S. Jacobs/NCEP	 2/98						*
C* S. Jacobs/NCEP	 4/98	Removed call to DCGAUS for cyclic end pt*
C* I. Durham/GSC	 4/98	Added routine to eliminate double points*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
C*
	REAL		xpt (*), ypt (*), xcv (*), ycv (*)
C*
	INTEGER		npt (LLMXPT)
	REAL		amtrx (LLMXPT,3), coeff (LLMXPT,2),
     +		 	chord (LLMXPT), xtng (LLMXPT), ytng (LLMXPT),
     +                  xtmp(LLMXPT), ytmp(LLMXPT) 
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for identical points and eliminate
C
	xtmp(1) = xpt(1)
	ytmp(1) = ypt(1)
	i = 2
	k = 2
	ntmp = np
	DO WHILE ( k .le. np )
	   IF ( ( xpt(i) .eq. xpt(i-1) ) .and.
     +          ( ypt(i) .eq. ypt(i-1) ) ) THEN
	      xtmp(k) = xpt(i+1)
	      ytmp(k) = ypt(i+1)
	      i   = i + 2
	      ntmp = ntmp - 1
	   ELSE
	      xtmp(k) = xpt(i)
	      ytmp(k) = ypt(i)
	      i = i + 1
	   END IF
	   k = k + 1
	END DO
C
C*	Zero fill the working matrices.
C
	DO  i = 1, LLMXPT
	    DO  j = 1, 2
		coeff (i,j) = 0.0
	    END DO
	    DO  j = 1, 3
		amtrx (i,j) = 0.0
	    END DO
	    chord (i) = 0.0
	END DO
C
C*	If the requested end point type is cyclic or the end points
C*	are the same, set the end point type to cyclic. Otherwise, 
C*	set the end point type to relaxed.
C
C*	Commented out checks for cyclic end points. This needs to be
C*	fixed later.
C
C--	IF ( ( itype .eq. 2 ) .or.
C--     +     ( ( xtmp(1) .eq. xtmp(ntmp) ) .and.
C--     +       ( ytmp(1) .eq. ytmp(ntmp) ) ) ) THEN
C--	   jtype = 2
C--	ELSE
	   jtype = 1
C--	END IF
C
C*	Fill the working matrices with the coefficient values.
C
	CALL DMATRX ( jtype, ntmp, xtmp, ytmp, dens, amtrx, coeff,
     +		      chord, npt, nout, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Compute the smooth curve.
C
C*	Commented out checks for cyclic end points. This needs to be
C*	fixed later.
C
C--	IF  ( jtype .eq. 2 )  THEN
C
C*	    Use cyclic conditions for the end points.
C
C--	    CALL DCGAUS ( ntmp, amtrx, coeff, xtng, ytng, ier )
C--	  ELSE
C
C*	    Use relaxed conditions for the end points.
C
	    CALL DGAUSS ( ntmp, amtrx, coeff, xtng, ytng, ier )
C--	END IF
C
C*	Compute the points for the smooth curve.
C
	CALL DESPLN ( ntmp, xtmp, ytmp, chord, xtng, ytng, npt,
     +		      xcv, ycv, ier )
C*
	RETURN
	END
