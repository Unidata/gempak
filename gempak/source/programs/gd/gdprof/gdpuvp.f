	SUBROUTINE GDPUVP ( gvect, u, v, y, npts, wind, winpos, filtfc,
     +                      windxn, windyn, refvec, iret )
C************************************************************************
C* GDPUVP								*
C*									*
C* This subroutine draws a wind profile in GDPROF.			*
C*									*
C* GDPUVP ( GVECT, U, V, Y, NPTS, WIND, WINPOS, FILTFC, WINDXN, WINDYN, *
C*          REFVEC, IRET )						*
C*									*
C*									*
C* Input parameters:							*
C*      GVECT           CHAR*           Wind function			*
C*      U(*)            REAL		U components			*
C*      V(*)            REAL            V components			*
C*      Y(*)            REAL            Vertical position		*
C*      NPTS		INTEGER		Number of levels		*
C* 	WIND		CHAR*		Wind display specification	*
C*      WINPOS          CHAR*           Wind plotting position		*
C*      FILTFC	 	REAL            Factor to filter wind		*
C*      WINDXN		REAL		Wind barb size in x		*
C*      WINDYN         	REAL		Wind barb size in y		*
C*      REFVEC		CHAR*		Reference arrow 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/GSC         12/89   					*
C* K. Brill/GSC          1/90		Fixed wind position problem	*
C* J. Nielsen/TAMU	11/91		Added filter factor		*
C* K. Brill/NMC		01/93		Added reference arrow		*
C* L. Sager/NMC		 7/93		Added REFVEC parameter		*
C* S. Jacobs/EAI	10/93		Changed call to IN_RVEC and	*
C*					   plotting of reference arrow	*
C* S. Jacobs/NMC	 8/94		Changed IN_RVEC to GG_RVEC	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gvect, wind, winpos, refvec
	REAL		u (*), v (*), y (*), filtfc
C*
	LOGICAL		knots
C*
	CHARACTER       gv*128, wintyp*1, winuni*1
	CHARACTER	defstr*12
	REAL		xwind (LLMXLV), ywind ( LLMXLV ),
     +                  spd (LLMXLV), dir (LLMXLV)
C*
	INCLUDE 'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Get input wind position.
C
	CALL ST_NUMB ( winpos, iwposn, ier )
	IF ( ( iwposn .lt. 1 ) .or. ( iwposn .gt. 3 ) ) iwposn = 1
C
C*	Get wind input for display type and color number.
C
	knots = .false.
	CALL ST_LCUC ( gvect, gv, ier )
	iuck = index ( gv, 'KNTV')
	CALL IN_WIND  ( wind, wintyp, winuni, iwncol, iret )
	IF ( winuni .eq. 'K' .and. iuck .eq. 0 ) knots = .true.
C
C*	Plot winds.
C
	IF  ( ( iwncol .gt. 0 ) .and. ( npts .gt. 0 ) )  THEN
	    CALL GSCOLR  ( iwncol, ier )
C
C*	    Transform y values to N coordinates.
C
	    CALL GQGRAF  ( ii, jj, yx, x1, y1, x2, y2, ier )
	    DO  i = 1, npts
		xwind (i) = x2
	    END DO
	    CALL GTRANS  ( 'M', 'N', npts, xwind, y, xwind,
     +			   ywind, ier )
C
C*	    Get coordinate at edge of plotting area.
C
	    CALL GQBND  ( 'P', x1, y1, x2, y2, ier )
C
C*	    Calculate coordinate for wind in N coordinates.
C
	    xwind (1) = x2 + 1.5 * windxn * FLOAT ( iwposn )
	    DO  i = 2, npts
		xwind (i) = xwind (1)
	    END DO
C
C*	Convert components to speed and direction.
C
	    kb = 1
	    ke = npts
	    indx = 0
	    DO k = kb, ke
	      IF ( ERMISS ( u ( k ) ) .or.
     +             ERMISS ( v ( k ) ) ) THEN
	      ELSE
	        dd = ATAN2 ( -u ( k ), -v ( k ) ) * RTD
	        sp = sqrt ( u ( k ) * u ( k ) +
     +		            v ( k ) * v ( k ) )
	        IF ( knots ) sp = sp * 1.94
	        indx = indx + 1
	        spd ( indx ) = sp
 	        dir ( indx ) = dd
	        xwind ( indx ) = xwind ( k )
	        ywind ( indx ) = ywind ( k )
	      END IF
	    END DO
	    nout = indx
C
C*	    Draw wind barbs or arrows.
C
	    IF  ( wintyp .eq. 'B' )  THEN
		IF  ( filtfc .ne. 0. )  THEN
		    brbftr = windyn * 0.6 * filtfc
		    nout = 1
		    yold = ywind (1)
	            DO  i = 2, indx
	                IF ( ( ywind (i) - yold ) .ge. brbftr )  THEN
			    nout = nout + 1
			    ywind ( nout ) = ywind (i)
			    spd ( nout ) = spd ( i )
			    dir ( nout ) = dir ( i )
			    yold = ywind (i)
			END IF
		    END DO
		    CALL GBARB  ( 'V', nout, xwind, ywind, spd,
     +				   dir, ier )
		  ELSE
		    CALL GBARB  ( 'V', nout, xwind, ywind, spd,
     +				   dir, ier )
		END IF
	      ELSE
		CALL GARRW  ( 'V', nout, xwind, ywind, spd,
     +			       dir, ier )
C
C*		Plot reference arrow if arrows were requested.
C*		Parse the parameter REFVEC and draw the arrow.
C
		IF  ( winuni .ne. 'N' )  THEN
		    IF  ( winuni .eq. 'K' )  defstr = 'kts'
		    IF  ( winuni .eq. 'M' )  defstr = 'm/s'
		    CALL GG_RVEC  ( refvec, defstr, ier )
		END IF
C
	    END IF
	END IF
C*
	RETURN
	END

