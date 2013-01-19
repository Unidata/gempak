	FUNCTION PS_SWET  ( t850, td850, t500, spd850, spd500, dir850, 
     +			    dir500 )
C************************************************************************
C* PS_SWET 								*
C*									*
C* This function computes the SWEAT index.  Winds must be input in	*
C* meters/sec.  They are converted to knots for the computation.	*
C*									*
C*     SWET = 12 * TD850 + 20 * TERM2 + 2 * SKT850 + SKT500 + SHEAR	*
C*									*
C*          TERM2  = MAX ( TOTL - 49, 0 )				*
C*          TOTL   = Total totals index					*
C*          SKT850 = 850 mb wind speed in knots				*
C*          SKT500 = 500 mb wind speed in knots				*
C*          SHEAR  = 125 * [SIN ( DIR500 - DIR850 ) + .2]		*
C*									*
C* If TD850 is negative, then TD850 is set to 0.  SHEAR is set to 0	*
C* if any of the following conditions is met:				*
C*									*
C*      wind direction at 850mb is < 130 or > 250			*
C*      wind direction at 500mb is < 210 or > 310			*
C*      DIR500 - DIR850 < 0						*
C*      SKT500 < 15  or  SKT850 < 15 knots				*
C*									*
C* Miller, R.C., 1972: Notes on Severe Storm Forecasting Procedures of	*
C* the Air Force Global Weather Central, AWS Tech. Report 200.		*
C*                                                                      *
C* REAL PS_SWET  ( T850, TD850, T500, SPD850, SPD500, DIR850, DIR500 )	*
C*									*
C* Input parameters:							*
C*	T850		REAL		850 mb temperature at 850 mb	*
C*	TD850		REAL		850 mb dewpoint at 850 mb	*
C*	T500		REAL		500 mb temperature at 500 mb	*
C*	SPD850		REAL		850 mb wind speed in m/sec	*
C*	SPD500		REAL		500 mb wind speed in m/sec	*
C*	DIR850		REAL		850 mb wind direction		*
C*	DIR500		REAL		500 mb wind direction		*
C*									*
C* Output parameters:							*
C*	PS_SWET		REAL		SWET index			*
C**	                                                            	*
C* Log:									*
C* P. Kocin/GSFC	1980						*
C* M. Goodman/RDS	11/84	Corrected errors			*
C* M. desJardins/GSFC	 3/88	Documentation				*
C* J. Whistler/NCEP	 1/96	Changed speed check to use knots	*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ERMISS ( t850 ) .or. ERMISS ( td850 ) .or. ERMISS ( t500 ) 
     +		.or. ERMISS ( spd850 ) .or. ERMISS ( spd500 ) .or. 
     +		     ERMISS ( dir850 ) .or. ERMISS ( dir500 ) )  THEN
	    PS_SWET = RMISSD
	  ELSE
C
C*	    Convert meters per second to knots.
C
	    skt850 = PR_MSKN ( spd850 )
	    skt500 = PR_MSKN ( spd500 )
C
C*	    Compute the total totals index.  If < 49, set term to zero.
C
	    total = PS_TOTL ( t850, td850, t500 )
	    term2 = total - 49.
	    IF  ( total .lt. 49. )  term2 = 0.
C
C*	    Compute shear term.
C
	    dif   = dir500 - dir850
	    s     = SIN ( dif * DTR )
	    shear = 125. * ( s + .2 )
C
C*	    Make various wind checks.
C
	    IF ((dir850 .lt. 130.) .or. (dir850 .gt. 250.)) shear = 0.
	    IF ((dir500 .lt. 210.) .or. (dir500 .gt. 310.)) shear = 0.
	    IF ((skt500 .lt. 15.)  .or. (skt850 .lt. 15. )) shear = 0.
	    IF (dif .le. 0.) shear = 0.
C
C*	    Check for subzero dewpoint.
C
	    dwp850 = td850
	    IF  ( dwp850 .lt. 0. )  dwp850 = 0.
C
C*	    Calculate SWEAT index.
C
	    PS_SWET = 12. * dwp850 + 20. * term2 + 2. * skt850 + 
     +		      skt500 + shear
	END IF
C*
	RETURN
	END                 
