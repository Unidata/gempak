	SUBROUTINE SNPPLT  ( temp, ntemp, dwpt, ndwpt, wind, nwind,
     +                       ip1arr, ip2arr, iwncol, iwloc, windxn,
     +			     windyn, filtfc, wintyp, mkcolr, iret )
C************************************************************************
C* SNPPLT								*
C*									*
C* This subroutine plots profile and wind data for SNPROF.		*
C*									*
C* SNPPLT  ( TEMP, NTEMP, DWPT, NDWPT, WIND, NWIND, IP1ARR, IP2ARR,	*
C*           IWNCOL, IWLOC, WINDXN, WINDYN, FILTFC, WINTYP, MKCOLR,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	TEMP (NTEMP)	REAL		Array of temperatures		*
C*	NTEMP		INTEGER		Number of temperatures		*
C*	DWPT (NDWPT)    REAL		Array of dewpt temperatures	*
C*	NDWPT		INTEGER         Number of dewpt temperatures	*
C*	WIND (NWIND)	REAL		Array of winds			*
C*      NWIND		INTEGER		Number of winds			*
C*	IP1ARR (5)	INTEGER		Line info for parm 1		*
C*	IP2ARR (5)	INTEGER		Line info for parm 2		*
C*	IWNCOL		INTEGER		Color for wind			*
C*	IWLOC		INTEGER		Location for wind		*
C*	WINDXN		REAL		X wind size			*
C*	WINDYN		REAL		Y wind size			*
C*	FILTFC		REAL		Filter factor			*
C*	WINTYP		CHAR*1		Wind type (B or A)		*
C*	MKCOLR          INTEGER		Marker color number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC          7/90   Moved data ingest code to SNPDTW	*
C* J. Nielsen/TAMU	11/91	Added filter factor
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wintyp
	INTEGER		ip1arr (*), ip2arr (*)
	REAL		filtfc
C*
	REAL		wind (LLMXLV,3), temp (LLMXLV,2)
	REAL		dwpt  (LLMXLV,2), xwind (LLMXLV), ywind (LLMXLV)
	CHARACTER	wt*1
C*
C----------------------------------------------------------------------
	iret  = 0
C
	IF  ( ( ip1arr (1) .eq. 0 ) .and. ( ip2arr (1) .eq. 0 ) .and.
     +	      ( iwncol .eq. 0 ) )  RETURN
C
C*	Save current line characteristics.
C
	CALL GQLINE  ( i1, i2, i3, i4, ier )
C
C*	Draw temperature line.
C
	IF  ( ( ip1arr (1) .gt. 0 ) .and. ( ntemp .gt. 0 ) )  THEN
C
C*	    Set the color and set the line type.
C
	    CALL GSCOLR  ( ip1arr (1), ier )
	    CALL GSLINE  ( ip1arr (2), 0, ip1arr (3), 0, ier )
C
C*	    Draw temperature line.
C
	    CALL GLINE  ( 'M', ntemp, temp (1,2), temp (1,1), ier )
C
C*	    Add markers.
C
	    IF  ( mkcolr .gt. 0 )  THEN
	        CALL GMARK   ( 'M', ntemp, temp (1,2), temp (1,1), ier )
	    END IF
	END IF
C
C*	Draw dewpoint line.
C
	IF  ( ( ip2arr (1) .gt. 0 ) .and. ( ndwpt .gt. 0 ) )  THEN
C
C*	    Set the color. Set the line type.
C
	    CALL GSCOLR  ( ip2arr (1), ier )
	    CALL GSLINE  ( ip2arr (2), 0, ip2arr (3), 0, ier )
	    CALL GLINE   ( 'M', ndwpt, dwpt (1,2), dwpt (1,1), ier )
C
C*	    Add markers.
C
	    IF  ( mkcolr .gt. 0 )  THEN
	        CALL GMARK  ( 'M', ndwpt, dwpt (1,2), dwpt (1,1), ier )
	    END IF
	END IF
C
C*	Reset the line type.
C
	CALL GSLINE  ( i1, 0, i3, 0, ier )
C
C*	Plot winds.
C
	IF  ( ( iwncol .gt. 0 ) .and. ( nwind .gt. 0 ) )  THEN
	    CALL GSCOLR  ( iwncol, ier )
	    wt = wintyp
C
C*	    Transform y values to N coordinates.
C
	    CALL GQGRAF  ( ii, jj, yx, x1, y1, x2, y2, ier )
	    DO  i = 1, nwind
		xwind (i) = x2
	    END DO
	    CALL GTRANS  ( 'M', 'N', nwind, xwind, wind (1,1), xwind,
     +			   ywind, ier )
C
C*	    Get coordinate at edge of plotting area.
C
	    CALL GQBND  ( 'P', x1, y1, x2, y2, ier )
C
C*	    Calculate coordinate for wind in N coordinates.
C
	    xwind (1) = x2 + 1.5 * windxn * FLOAT ( iwloc )
	    DO  i = 2, nwind
		xwind (i) = xwind (1)
	    END DO
C
C*	    Draw wind barbs or arrows.
C
	    IF  ( wt .eq. 'B' )  THEN
		IF  ( filtfc .ne. 0. )  THEN
		    brbftr = windyn * 0.6 * filtfc
		    nout = 1
		    yold = ywind (1)
	            DO  i = 2, nwind
	                IF ( ( ywind (i) - yold ) .ge. brbftr )  THEN
			    nout = nout + 1
			    ywind ( nout ) = ywind (i)
			    wind ( nout, 2 ) = wind ( i, 2 )
			    wind ( nout, 3 ) = wind ( i, 3 )
			    yold = ywind (i)
			END IF
		    END DO
		    CALL GBARB  ( 'V', nout, xwind, ywind, wind (1,3),
     +				   wind (1,2), ier )
		  ELSE
		    CALL GBARB  ( 'V', nwind, xwind, ywind, wind (1,3),
     +				   wind (1,2), ier )
		END IF
	      ELSE
		CALL GARRW  ( 'V', nwind, xwind, ywind, wind (1,3), 
     +			       wind (1,2), ier )
	    END IF
	END IF
C*
	RETURN
	END
