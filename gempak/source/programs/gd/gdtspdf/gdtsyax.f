	SUBROUTINE GDTSYAX  ( ptype, yaxis, timfnd, ntms, gvcord, 
     +                        glevel, gfunc, gpoint, ymin, ymax, rgx, 
     +                        rgy, plat, plon, iyaxis, ratio, ystrt, 
     +                        ystop, ylbl, nylbl, rmargn,
     +	 		      iylbsf, iyglsf, iytmsf, iret )
C************************************************************************
C* GDTSYAX								*
C*									*
C* This subroutine returns the parameters to use for the y axis 	*
C* in GDTSPDF								*
C*									*
C* GDTSYAX ( PTYPE, YAXIS, TIMFND, NTMS, GVCORD, GLEVEL, GFUNC, GPOINT, *
C*          YMIN, YMAX, RGX, RGY, PLAT, PLON, IYAXIS, RATIO, YSTRT,  	*
C*          YSTOP, YLBL, NYLBL, RMARGN, IYLBSF, IYGLSF, IYTMSF, IRET )	*
C*									*
C* Input parameters:							*
C*	PTYPE		CHAR*		Y axis type			*
C*	YAXIS		CHAR*		Ymin / ymax / yinc		*
C*      TIMFND		CHAR*		List of times			*
C*	NTMS		INTEGER		Number of times			*
C*      GVCORD          CHAR*   	Grid vertical coordinate	*
C*      GLEVEL          CHAR*   	Grid level                      *
C*      GFUNC           CHAR*   	User input function             *
C*      GPOINT          CHAR*   	Grid point                      *
C*									*
C* Output parameters:							*
C*	YMIN		REAL		Y min				*
C*	YMAX		REAL		Y MAX				*
C*      RGX             REAL            X grid coordinate               *
C*      RGY             REAL            Y grid coordinate               *
C*      PLAT            REAL            Latitude                        *
C*      PLON            REAL            Longitude                       *
C*	IYAXIS		INTEGER		Y axis integer type		*
C*	RATIO		REAL		Height to width ratio		*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*	YLBL  (NYLBL)	REAL		Y axis label values		*
C*	NYLBL		INTEGER		Number of y axis labels		*
C*	RMARGN (4)	REAL		Margins				*
C*	IYLBSF		INTEGER		Label frequency			*
C*	IYGLSF		INTEGER		Grid line frequency		*
C*	IYTMSF          INTEGER		Tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid axis type		*
C*                                      -11 = invalid axis request	*
C**									*
C* Log:									*
C* M. Li/SAIC		10/07						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ptype, yaxis, timfnd(*) 
        CHARACTER*(*)   gvcord, glevel, gfunc, gpoint
	REAL		ylbl (*), rmargn (*)
C*
	CHARACTER	dattim (2)*20,  time (2)*20, tmpstr*128
	CHARACTER	dparm1*(LLMXLN),dparm2*(LLMXLN), pfunc*80
        CHARACTER	pparm*60
	INTEGER         lev(2)
        REAL            grid (LLMXGD)
	LOGICAL		proces, gottm
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Compose function string embedding GFUNC 
C
	CALL ST_LSTR ( gfunc, len, ier )
	tmpstr = '(' // gfunc(:len) // ')'
	CALL ST_LSTR ( tmpstr, len1, ier )
	dparm1 = 'ENS_SMAX' // tmpstr(:len1)
	dparm2 = 'ENS_SMIN' // tmpstr(:len1)
C
C*	Compute Y max and  Y min
C
	DO ii = 1, ntms
C
C*          Get the next time to process from the time server.
C
            CALL DG_NTIM ( .true., .true., time, gottm, ier )
	    proces = ( ier .eq. 0 .and. gottm )
	    IF ( .not. proces ) continue
C
C*          Find plotting location. 
C
            CALL DG_CXGP  ( gpoint, ' ', 1, np, rgx, rgy, plat,
     +			    plon, iret )
C
C*          Compute Y max   
C
            CALL DG_GRID ( timfnd(ii), glevel, gvcord, dparm1,
     +                 pfunc, grid, kx, ky,
     +                 dattim, lev, jvcord, pparm, ier )

            IF ( ier .ge. 0 )
     +          CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, grid, 
     +                          ymax0, ier )
C
	    IF ( ii .eq. 1 ) THEN
		ymax = ymax0
	      ELSE
		ymax = AMAX1(ymax, ymax0)
	    END IF
C
C*          Compute Y min
C
            CALL DG_GRID ( timfnd(ii), glevel, gvcord, dparm2,
     +                 pfunc, grid, kx, ky,
     +                 dattim, lev, jvcord, pparm, ier )

            IF ( ier .ge. 0 )
     +          CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, grid, 
     +                          ymin0, ier )
C
            IF ( ii .eq. 1 ) THEN
                ymin = ymin0
              ELSE
                ymin = AMIN1(ymin, ymin0)
            END IF
	END DO
C
C*	Get information about Y axis
C
	CALL GDPTYAX  ( ptype, yaxis, ymin, ymax,
     +                  iyaxis, ratio, ystrt, ystop, ylbl,
     +                  nylbl, rmargn, iylbsf, iyglsf,
     +                  iytmsf, iret )

C*
	RETURN
	END
