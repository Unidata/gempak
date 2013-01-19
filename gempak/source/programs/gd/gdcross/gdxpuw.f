	SUBROUTINE GDXPUW ( gvect, u, w, x, y, nx, nv, wind, points,
     +                      refvec, iret )
C************************************************************************
C* GDXPUW								*
C*									*
C* This subroutine plots the winds on a cross section plane.  The 	*
C* input arrays are changed.						*
C*									*
C*									*
C* GDXPUW ( GVECT, U, W, X, Y, NX, NV, WIND, POINTS, REFVEC, IRET )	*
C*									*
C* Input parameters:							*
C*      GVECT                 CHAR*     Wind function			*
C*      U      (NX, NV)	      REAL      Array of u components		*
C*	W      (NX, NV)	      REAL	Array of vertical or v comps	*
C*      X      (NX, NV)	      REAL      Array of horz posit in M coord	*
C*      Y      (NX, NV)       REAL      Array of vert posit in M coord	*
C*      NX                    INTEGER   Number of points in x 		*
C*      NV                    INTEGER   Number of points in y		*
C*      WIND                  CHAR*	Wind display type input		*
C*      POINTS		      CHAR*	Display points (not used)	*
C*	REFVEC		      CHAR*	Reference wind			*
C*									*
C* Output parameters:							*
C*	IRET		      INTEGER	Return code			*
C*					  0 = normal return		*
C*					      same as for GBARB/GARRW	*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89  						*
C* K. Brill/GSC         12/89	Add conversion to knots			*
C* J. Whistler/SSAI	 5/91	Change plot from 'M' to 'V'		*
C* K. Brill/NMC		01/93	Get 'N' (no ref arrow) from WINUNI	*
C* L. Sager/NMC		 7/93   Added REFVEC parameter			*
C* S. Jacobs/EAI	10/93	Changed call to IN_RVEC and plotting of *
C*				   reference arrow			*
C* S. Jacobs/NMC	 8/94	Changed IN_RVEC to GG_RVEC		*
C* S. Gilbert/NCEP	 8/07	Redimensioned x1() and y1()		*
C************************************************************************
	INCLUDE 'GEMPRM.PRM'
C*
	CHARACTER*(*)	gvect, wind, points, refvec
        REAL 		u (*), w (*), x (*), y (*)
C*
	CHARACTER  	gv*128, wintyp*2, winuni*1
	CHARACTER	defstr*12
	LOGICAL         knots
	REAL 		x1 (LLMXLV*MXXPTS), y1 (LLMXLV*MXXPTS)
C*
	INCLUDE 'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Decode the WIND input.
C
	knots = .false.
	CALL ST_LCUC ( gvect, gv, ier )
	iuck = index ( gv, 'KNTV')
        CALL IN_WIND ( wind, wintyp (1:1), winuni, icl, ier )
	IF ( winuni .eq. 'K' .and. iuck .eq. 0 ) knots = .true.
        CALL GSCOLR ( icl, ier )
C
C*	Reload wind arrays converting to speed and direction and
C*	eliminating missing values.
C
	kb = 1
	ke = nv
	ib = 1
	ie = nx
	kstp = 1
	istp = 1
	indx = 0
	DO k = kb, ke, kstp
	  DO i = ib, ie, istp
	    indin = ( k - 1 ) * nx + i
	    IF ( ERMISS ( u ( indin ) ) .or.
     +           ERMISS ( w ( indin ) ) ) THEN
	    ELSE
	      dd = ATAN2 ( -u ( indin ), -w ( indin ) ) * RTD
	      sp = sqrt ( u ( indin ) * u ( indin ) +
     +		          w ( indin ) * w ( indin ) )
	      IF ( knots ) sp = sp * 1.94
	      indx = indx + 1
	      u ( indx ) = sp
 	      w ( indx ) = dd
	      x ( indx ) = x ( indin )
	      y ( indx ) = y ( indin )
	    END IF
	  END DO
	END DO
C*
	IF ( indx .gt. 0 ) THEN
C
C*	  Transform the plot field.
C
 	  CALL GTRANS ( 'M', 'N', indx, x, y, x1, y1, iret )
C
C*	  Plot the winds.
C
	  IF ( wintyp ( 1:1 ) .eq. 'A' ) THEN
C
C*	    Plot the arrows.
C
	    CALL GARRW ( 'V', indx, x1, y1, u, w, iret )
C
C*	    Plot reference arrow if arrows were requested.
C*	    Parse the parameter REFVEC and draw the arrow.
C
	    IF  ( winuni .ne. 'N' )  THEN
		IF  ( winuni .eq. 'K' )  defstr = 'kts'
		IF  ( winuni .eq. 'M' )  defstr = 'm/s'
		CALL GG_RVEC  ( refvec, defstr, ier )
	    END IF
	  ELSE
C
C*	    Plot the barbs.
C
	    CALL GBARB ( 'V', indx, x1, y1, u, w, iret )
	  END IF
	END IF	
	RETURN
	END
