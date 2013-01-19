	SUBROUTINE CV_PRMT ( np, xpt, ypt, dens, maxpts, crvscl,
     +			     istrt, iend, nout, xcv, ycv, iret )
C************************************************************************
C* CV_PRMT								*
C*									*
C* This subroutine performs a parametric curve fit to NP data points.	*
C* ISTRT and IEND define the range of points to return in XCV and YCV.	*
C* If ISTRT and IEND are outside of the number of original points, then	*
C* all the new points are returned.  Otherwise, the curve between ISTRT	*
C* and IEND is returned.						*
C*									*
C* "Parametric curve fitting:  An alternative to Lagrange interpolation	*
C* and splines", Y. Akyildiz, Computers in Physics, Vol 8, No 6,	*
C* Nov/Dec 1994, pp 722-729.						*
C*									*
C* CV_PRMT ( NP, XPT, YPT, DENS, MAXPTS, CRVSCL, ISTRT, IEND,		*
C*	     NOUT, XCV, YCV, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of input points		*
C*	XPT (NP)	REAL		X input coordinates		*
C*	YPT (NP)	REAL		Y input coordinates		*
C*	DENS		REAL		Density of intermediate points	*
C*	MAXPTS		INTEGER		Maximum number of output points	*
C*	CRVSCL		REAL		Device curve scaling factor	*
C*	ISTRT		INTEGER		Start of segment to return	*
C*	IEND		INTEGER		End of segment to return	*
C*									*
C* Output parameters:							*
C*	NOUT		INTEGER		Number of output points		*
C*	XCV (NOUT)	REAL		X evaluated coordinates		*
C*	YCV (NOUT)	REAL		Y evaluated coordinates		*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* S. Jacobs/NCEP	 4/98						*
C* S. Jacobs/NCEP	 4/98	Fixed a bug with short segments		*
C* S. Jacobs/NCEP	 7/98	Added MAXPTS to call seq and checks	*
C* T. Lee/GSC		 7/98	Renamed DPRMTC; Added CRVSCL to	call seq*
C* S. Jacobs/NCEP	 8/98	Added ISTRT & IEND to calling sequence	*
C* S. Jacobs/NCEP	 9/98	Do not comp curve if <= 2 unique points	*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* S. Jacobs/NCEP	 5/99	Fixed calc of num of pts per sub-segment*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		xpt (*), ypt (*), xcv (*), ycv (*)
C*
	INTEGER		npt (LLMXPT)
	REAL		xtmp(0:LLMXPT), ytmp(0:LLMXPT)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the start and end points of the return segment.
C
	kstrt = istrt
	kend  = iend
	IF  ( ( kstrt .lt. 1 ) .or. ( kstrt .gt. np ) )  kstrt = 1
	IF  ( ( kend  .lt. 1 ) .or. ( kend  .gt. np ) )  kend  = np
	IF  ( kstrt .ge. kend )  THEN
	    kstrt = 1
	    kend  = np
	END IF
C
C*	Check for identical points and eliminate.
C
	k = 1
	xtmp(k) = xpt(1)
	ytmp(k) = ypt(1)
	jstrt = kstrt
	jend  = kend
C
	DO  i = 2, np
	    IF  ( ( xtmp(k) .eq. xpt(i) ) .and.
     +		  ( ytmp(k) .eq. ypt(i) ) ) THEN
		IF  ( kstrt .ge. i )  jstrt = jstrt - 1
		IF  ( kend  .ge. i )  jend  = jend  - 1
	      ELSE
		k = k + 1
		xtmp(k) = xpt(i)
		ytmp(k) = ypt(i)
	    END IF
	END DO
	ntmp = k
C
C*	Check for all points the same.
C
	IF  ( ntmp .le. 2 )  THEN
	    DO  m = 1, np
		xcv(m) = xpt(m)
		ycv(m) = ypt(m)
	    END DO
	    nout = np
	    RETURN
	END IF
C
C*	Get the number of intermediate points for each segment.
C*	Compute the chord length of each segment.
C
	nout = 0
	DO  i = 1, ntmp-1
	    chord  = SQRT ( (xtmp(i+1)-xtmp(i))**2 +
     +			    (ytmp(i+1)-ytmp(i))**2 )
	    npt(i) = INT ( chord / ( crvscl / dens ) ) + 1
	    nout = nout + npt(i)
	END DO
	nout = nout + ntmp
C
C*	If the number of output points is greater than the max allowed,
C*	set the output points to the input points and return.
C
	IF  ( nout .gt. maxpts )  THEN
	    iret = -1
	    DO  m = 1, np
		xcv(m) = xpt(m)
		ycv(m) = ypt(m)
	    END DO
	    nout = np
	    RETURN
	END IF
C
C*	Get the addtional end points, if necessary.
C
	IF  ( ( xtmp(1) .eq. xtmp(ntmp) ) .and.
     +	      ( ytmp(1) .eq. ytmp(ntmp) ) )  THEN
	    xtmp(0) = xtmp(ntmp-1)
	    ytmp(0) = ytmp(ntmp-1)
	    xtmp(ntmp+1) = xtmp(2)
	    ytmp(ntmp+1) = ytmp(2)
	  ELSE
	    xtmp(0) = ( 5*xtmp(1) - 4*xtmp(2) + xtmp(3) ) / 2.0
	    ytmp(0) = ( 5*ytmp(1) - 4*ytmp(2) + ytmp(3) ) / 2.0
	    xtmp(ntmp+1) = ( 5*xtmp(ntmp) - 4*xtmp(ntmp-1) +
     +			     xtmp(ntmp-2) ) / 2.0
	    ytmp(ntmp+1) = ( 5*ytmp(ntmp) - 4*ytmp(ntmp-1) +
     +			     ytmp(ntmp-2) ) / 2.0
	END IF
C
C*	Find the points for the smooth curve.
C
	xcv (1) = xtmp(jstrt)
	ycv (1) = ytmp(jstrt)
	m = 1
	DO  k = jstrt, jend-1
	    DO  j = 1, npt(k)
		t = FLOAT(j) / FLOAT(npt(k))
		m = m + 1
		xcv(m) = xtmp(k)
     +			 + 0.5*(xtmp(k+1)-xtmp(k-1))*t
     +			 - 0.5*(xtmp(k+2)-4*xtmp(k+1)+
     +			        5*xtmp(k)-2*xtmp(k-1))*(t**2)
     +			 + 0.5*(xtmp(k+2)-3*xtmp(k+1)+
     +			        3*xtmp(k)-xtmp(k-1))*(t**3)
		ycv(m) = ytmp(k)
     +			 + 0.5*(ytmp(k+1)-ytmp(k-1))*t
     +			 - 0.5*(ytmp(k+2)-4*ytmp(k+1)+
     +			        5*ytmp(k)-2*ytmp(k-1))*(t**2)
     +			 + 0.5*(ytmp(k+2)-3*ytmp(k+1)+
     +			        3*ytmp(k)-ytmp(k-1))*(t**3)
	    END DO
	END DO
	nout = m
C*
	RETURN
	END
