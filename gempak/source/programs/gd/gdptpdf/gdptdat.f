	SUBROUTINE GDPTDAT  ( gdatim, gvcord, glevel, gfunc, prbtyp,
     +			     ymin, ymax, ylbl, nylbl, ystrt, ystop,
     +			     rgx, rgy, npts, x, y, parm, iret )
C************************************************************************
C* GDPTDAT								*
C*									*
C* This subroutine gets the data to plot for probabilities.		*
C*									*
C* GDPTDAT  ( GDATIM, GVCORD, GLEVEL, GFUNC, PRBTYP, YMIN, YMAX, 	*
C*            RGX, RGY, NPTS, X, Y, PARM, IRET )			*
C*									*
C* Input parameters:							*
C*	GDATIM		CHAR*		User input date/time		*
C*	GVCORD		CHAR*		User input vert coord		*
C*	GLEVEL		CHAR*		Grid level			*
C*	GFUNC		CHAR*		User input function		*
C*	PRBTYP		INTEGER		Probability type		*
C*      YMIN            REAL            Minumum Y                       *
C*      YMAX            REAL            Maximum Y                       *
C*      YLBL  (NYLBL)   REAL            Y axis label values             *
C*      NYLBL           INTEGER         Number of y axis labels         *
C*      YSTRT           REAL            Bottom y value                  *
C*      YSTOP           REAL            Top y value                     *
C*      RGX             REAL            X grid coordinate               *
C*      RGY             REAL            Y grid coordinate               *
C*									*
C* Output parameters:							*
C*	NPTS		INTEGER		Number of points		*
C*	X    (NPTS)	REAL		X coordinates			*
C*	Y    (NPTS)	REAL		Y coordinates			*
C*	PARM		CHAR*		Parameter name			*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC           08/07                                           *
C* M. Li/SAIC           01/08	Added a check for missing value		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, gvcord, glevel, gfunc, parm
	REAL		x (*), y (*), ylbl(*)
	INTEGER		prbtyp
C*
	CHARACTER       dattim (2)*20, pparm*12
	CHARACTER*(LLMXLN) dparm, pfunc, text
C*
        INTEGER         lev(2)
        REAL            grid (LLMXGD), z(0:nylbl)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	npts = nylbl 
C
C*	Increment in Y axis
C
	rinc = ABS ( ylbl(2) - ylbl(1) )
	IF  ( rinc .eq. 0 )  THEN
	    iret = -10
            CALL ER_WMSG  ( 'GDPTPDF', iret, ' ', ier )
	    RETURN
        END IF
C
C*	Compute the probability.
C
	CALL ST_LSTR ( gfunc, len, ier )
	CALL ST_LCUC ( gfunc, parm, ier ) 
	parm = 'PROB OF ' // parm(:len)

	DO ii = 0, nylbl 
	    IF ( ii .gt. 0 ) x(ii) = RMISSD
	    IF ( ii .gt. 0 ) THEN
		yy    = ylbl(ii)
	        y(ii) = ylbl(ii)
	      ELSE
		yy = ylbl(1) - rinc
	    END IF
C
	    CALL ST_RLCH ( yy, 6, text, ier )
	    CALL ST_LSTR ( text, len1, ier )
	    dparm = 'ENS_CPRB' // '(' // gfunc(:len) // '&' // 
     +              text(:len1) // ')'
    	    CALL DG_GRID ( gdatim, glevel, gvcord, dparm,
     +                 pfunc, grid, kx, ky, 
     +                 dattim, lev, jvcord, pparm, ier )
C

	    IF ( ier .ge. 0 ) THEN
		CALL GR_INTP (1, rgx, rgy, 1, kx, ky, grid, z(ii), ier)
C
	        IF ( (ii .gt. 0) .and. ( .not. ERMISS(z(ii)) ) ) THEN
		    x(ii) = z(ii)
		    IF ( (prbtyp .eq. 2) .and. ( .not. ERMISS(z(ii-1)) ) )
     +			 x(ii) = z(ii) - z(ii-1)
		END IF
	    END IF
C	
	END DO
C*
	RETURN
	END
