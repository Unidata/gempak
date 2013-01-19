	FUNCTION PR_IGRO  ( tmpc, sstc, sped )
C************************************************************************
C* PR_IGRO								*
C*									*
C* This function computes IGRO, the rate of ice accretion/growth of ice *
C* on a vessel in salt water, in units of inches per 3 hours (the WMO   *
C* standard).                                                           *
C* The formula used is:                                                 *
C*									*
C*       IGRO = ( A*pr + B*pr*pr + C*pr*pr*pr ) * CVFAC                 *
C*   where                                                              *
C*	A  = 2.73 * 10e-2                                               *
C*	B  = 2.91 * 10e-4                                               *
C*	C  = 1.84 * 10e-6                                               *
C*	pr = ( sped * ( -1.7 - tmpc ) ) / ( 1 + 0.4 * ( sstc + 1.7 ) )  *
C*	        (priesendorfer regression)                              *
C*   and                                                                *
C*      CVFAC = 1.1811, to convert cm/hr to in/3hr.                     *
C*									*
C* REAL PR_IGRO  ( TMPC, SSTC, SPED )					*
C*									*
C* Input parameters:							*
C*	TMPC		REAL	Observed surface air temperature (C)    *
C*	SSTC 		REAL	Observed sea-surface temperature (C)    *
C*	SPED		REAL	Observed wind speed (m/s)               *
C*									*
C* Output parameters:							*
C*	PR_IGRO		REAL	Rate of ice accretion (in/3hr)          *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/00						*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
C*
	PARAMETER	( A = .273E-1, B = .291E-3, C = .184E-5 )
      	PARAMETER	( CVFAC = 1.1811 )
C*
	LOGICAL		good
C*
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_IGRO = RMISSD
C
	IF ( ERMISS ( tmpc ) .or. ERMISS ( sstc ) .or.
     +       ERMISS ( sped ) ) THEN
	    RETURN
	END IF
C
C*	Check that all values are within valid ranges for this formula.
C
	good = .true.
	IF ( ( sped .lt. 0. ) .or. ( sped .gt. 50. ) ) good = .false.
	IF ( ( tmpc .lt. (-20.) ) .or. ( tmpc .gt. 0. ) ) good = .false.
	IF ( ( sstc .lt. (-1.7) ) .or. ( sstc .gt. 12.) ) good = .false.
C
	IF ( good ) THEN
	    pr  = ( sped * ( -1.7 - tmpc ) ) / 
     +            ( 1. + 0.4 * ( sstc + 1.7 ) )
	    pr2 = pr * pr
	    PR_IGRO = ( A * pr + B * pr2 + C * pr * pr2 ) * CVFAC
	END IF
C
	IF ( PR_IGRO .lt. 0. )  PR_IGRO = RMISSD
C*
	RETURN
	END
