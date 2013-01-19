	SUBROUTINE GDTSDAT ( gdfile, gdatim, gpoint, timfnd, ntms, 
     +			     gvcord, glevel, gfunc,
     +			     iprbty, ymin, ymax, ylbl, nylbl,
     +			     ystrt, ystop, rgx, rgy, grdout, iret )
C************************************************************************
C* GDTSDAT								*
C*									*
C* This subroutine computes probability as a function of time series	*
C* and gfunc values.							*
C*									*
C* GDTSDAT  ( GDFILE, GDATIM, GPOINT, TIMFND, NTMS, GVCORD, GLEVEL, 	*
C*            GFUNC, IPRBTY, YMIN, YMAX, YLBL, NYLBL, YSTRT, YSTOP, 	*
C*            RGX, RGY, GRDOUT, IRET )					*
C*									*
C* Input parameters:							*
C*	GDFILE          CHAR*           Grid file name                  *
C*      GDATIM          CHAR*           Grid time     			*
C*	GPOINT		CHAR*		Grid point			*
C*      TIMFND          CHAR*           List of times                   *
C*      NTMS            INTEGER         Number of times                 *
C*	GVCORD		CHAR*		User input vert coord		*
C*	GLEVEL		CHAR*		Grid level			*
C*	GFUNC		CHAR*		User input function		*
C*	IPRBTY		INTEGER		Probability type		*
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
C*	GRDOUT(NTMS, NYLBL) REAL	Grid data on output		*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC           10/07                                           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, gdatim, gpoint
	CHARACTER*(*)	gvcord, glevel, gfunc, timfnd (*)
	REAL		ylbl(*), grdout(ntms, nylbl)
C
	REAL 		x (LLMXLV), y (LLMXLV)
	CHARACTER	parm*32, time (2)*20
	LOGICAL         proces, gottm
C------------------------------------------------------------------------
	iret = 0
C
C*      Process the GDFILE input.
C
        CALL DG_NFIL ( gdfile, ' ', ier )
        IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'DG', ier, ' ', irr )
            RETURN 
        END IF
C
C*      Process the GDATTIM input; setup the time server.
C
        CALL DG_NDTM ( gdatim, ier )
        IF ( ier .ne. 0 ) THEN 
            CALL ER_WMSG ( 'DG', ier, gdatim, irr )
            RETURN 
        END IF
C
C*	Compute probability as function of time series
C
	DO ii = 1, ntms
C
C*          Get the next time to process from the time server.
C
            CALL DG_NTIM ( .true., .true., time, gottm, ier )
            proces = ( ier .eq. 0 .and. gottm )
C 
            IF ( .not. proces )  THEN
		DO jj = 1, nylbl
                    grdout(ii, jj) = RMISSD
                END DO
		continue
	    END IF
C
C*          Find plotting location.
C
            CALL DG_CXGP  ( gpoint, ' ', 1, np, rgx, rgy, plat,
     +                      plon, ier )
C
C*	    Compute the probability 
C
	    CALL GDPTDAT ( timfnd(ii), gvcord, glevel, gfunc,
     +                     iprbty, ymin, ymax, ylbl, nylbl,
     +                     ystrt, ystop, rgx, rgy,
     +                     npts, x, y, parm, ier )
	    IF ( ier .ne. 0 .or. .not. proces ) THEN
		DO jj = 1, nylbl 
                    grdout(ii, jj) = RMISSD 
                END DO
	      ELSE
	        DO jj = 1, npts
		    grdout(ii, jj) = x(jj)
		END DO
	    END IF
	END DO
C*
	RETURN
	END
