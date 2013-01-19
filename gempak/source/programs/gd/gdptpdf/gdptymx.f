	SUBROUTINE GDPTYMX  ( gdatim, gvcord, glevel, gfunc, gpoint,
     +			      rgx, rgy, plat, plon, ymax, ymin, iret )
C************************************************************************
C* GDPYAX								*
C*									*
C* This subroutine calculates maximum and minimum for the y axis 	*
C* in GDPTPDF.								*
C*									*
C* GDPYAX  ( GDATIM, GVCORD, GLEVEL, GFUNC, GPOINT, RGX, RGY, PLAT,     *
C*           PLON, YMAX, YMIN, IRET )					*
C*									*
C* Input parameters:							*
C*	GDATIM		CHAR*	Grid date/time  			*
C*	GVCORD       	CHAR*	Grid vertical coordinate		*
C*	GLEVEL        	CHAR*	Grid level				*
C*	GPOINT    	CHAR*	Grid point				* 
C*									*
C* Output parameters:							*
C*      RGX             REAL            X grid coordinate               *
C*      RGY             REAL            Y grid coordinate               *
C*      PLAT            REAL            Latitude                        *
C*      PLON            REAL            Longitude                       *
C*      YMAX            REAL            Maximum Y                       *
C*      YMIN            REAL            Minumum Y 			*
C*      IRET            INTEGER         Return code                     *
C**									*
C* Log:									*
C* M. Li/SAIC		08/07						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, gvcord, glevel, gfunc, gpoint
C*
	CHARACTER	dparm*128, pfunc*80, dattim (2)*20, pparm*60
C*
	INTEGER		lev(2)
	REAL		grid (LLMXGD)
C*
C------------------------------------------------------------------------


	iret = 0
	ymin = RMISSD
	ymax = RMISSD
C
C* 	Find plotting location.	
C
	CALL DG_CXGP  ( gpoint, ' ', 1, np, rgx, rgy, plat, plon, iret )
        IF ( iret .ne. 0 ) RETURN
C
C*	Compute Y max	
C
	CALL ST_LSTR ( gfunc, len, ier )
	dparm = 'ENS_SMAX' // '(' // gfunc(:len) // ')'
	CALL DG_GRID ( gdatim, glevel, gvcord, dparm,
     +                 pfunc, grid, kx, ky,
     +                 dattim, lev, jvcord, pparm, ier )

	IF ( ier .ge. 0 )
     +	    CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, grid, ymax, ier ) 
C
C*      Compute Y min   
C
        dparm = 'ENS_SMIN' // '(' // gfunc(:len) // ')'
        CALL DG_GRID ( gdatim, glevel, gvcord, dparm,
     +                 pfunc, grid, kx, ky,
     +                 dattim, lev, jvcord, pparm, ier )
        
        IF ( ier .ge. 0 )
     +      CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, grid, ymin, ier )     

C*
	RETURN
	END
