	SUBROUTINE DESPLN ( np, xpt, ypt, chord, xtng, ytng, npt,
     +			    xcv, ycv, iret )
C************************************************************************
C* DESPLN								*
C*									*
C* This subroutine evalutes a smooth curve using cubic splines.		*
C*									*
C* DESPLN ( NP, XPT, YPT, CHORD, XTNG, YTNG, NPT, XCV, YCV, IRET )	*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of input points		*
C*	XPT (NP)	REAL		X input coordinates		*
C*	YPT (NP)	REAL		Y input coordinates		*
C*	CHORD (NP)	REAL		Chord lengths			*
C*	XTNG (NP)	REAL		X comp of tangent vectors	*
C*	YTNG (NP)	REAL		Y comp of tangent vectors	*
C*	NPT (NP)	INTEGER		Number of points per segment	*
C*									*
C* Output parameters:							*
C*	XCV (NOUT)	REAL		X evaluated coordinates		*
C*	YCV (NOUT)	REAL		Y evaluated coordinates		*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* S. Jacobs/NCEP	 2/98						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		npt (*)
	REAL		xpt (*), ypt (*), chord(*), xtng (*), ytng (*),
     +			xcv (*), ycv (*)
C------------------------------------------------------------------------
	iret = 0
C
	k = 1
	DO  i = 1, np-1
C
C*	    Compute the coefficents for the polynomial.
C
	    f11 = xpt(i)
	    f12 = ypt(i)
C
	    f21 = xtng(i)
	    f22 = ytng(i)
C
	    q   = 1 / chord(i)
	    qs  = q ** 2
C
	    q1  = q  * 3.0 * ( xpt(i+1) - xpt(i) )
	    f31 = q  * ( q1 - 2.0*xtng(i) - xtng(i+1) )
	    q1  = q  * 3.0 * ( ypt(i+1) - ypt(i) )
	    f32 = q  * ( q1 - 2.0*ytng(i) - ytng(i+1) )
C
	    q1  = q  * 2.0 * ( xpt(i) - xpt(i+1) )
	    f41 = qs * ( q1 + xtng(i) + xtng(i+1) )
	    q1  = q  * 2.0 * ( ypt(i) - ypt(i+1) )
	    f42 = qs * ( q1 + ytng(i) + ytng(i+1) )
C
C*	    Compute the points along the smooth curve.
C
	    xcv(k) = xpt(i)
	    ycv(k) = ypt(i)
	    k = k + 1
	    IF  ( npt(i) .ne. 0 )  THEN
		delt = chord(i) / FLOAT(npt(i)+1)
		tt = 0.0
		DO  j = 1, npt(i)
		    tt = tt + delt
		    xcv(k) = f11 + f21*tt + f31*(tt**2) + f41*(tt**3)
		    ycv(k) = f12 + f22*tt + f32*(tt**2) + f42*(tt**3)
		    k = k + 1
		END DO
	    END IF
	END DO
C
	xcv(k) = xpt(np)
	ycv(k) = ypt(np)
C*
	RETURN
	END
