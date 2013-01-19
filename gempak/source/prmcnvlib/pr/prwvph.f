	FUNCTION PR_WVPH  ( powv, howv, group )
C************************************************************************
C* PR_WVPH								*
C*									*
C* This function computes WVPH, the combined wave period and wave       *
C* height, from POWV and HOWV.  A group number is prefixed to the       *
C* combined value to distinguish among instrument waves, wind waves,    *
C* predominant swell waves, and secondary swell waves.  On output, the  *
C* period of the waves is in seconds and the height of the waves is in  *
C* units determined by the calling routine.                             *
C* The following equation is used:                                      *
C*									*
C*       WVPH = ( GROUP * 10000 ) + ( POWV * 100 ) + HOWV               *
C*									*
C* REAL PR_WVPH  ( POWV, HOWV, GROUP )					*
C*									*
C* Input parameters:							*
C*	POWV		REAL	Wave period in seconds                  *
C*	HOWV 		REAL	Wave height (units from calling rtn)    *
C*	GROUP		REAL	Wave group number                       *
C*				  0 = no group				*
C*				  1 = instrument waves			*
C*				  2 = wind waves                        *
C*				  4 = predominant swell waves           *
C*				  5 = secondary swell waves             *
C*									*
C* Output parameters:							*
C*	PR_WVPH		REAL	Combined wave period and height         *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	12/98						*
C* D. Kidwell/NCEP	 2/99	Changed to use NINT; generalized        *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_WVPH = RMISSD
C
	IF ( ( .not. ERMISS ( powv ) ) .and. 
     +       ( .not. ERMISS ( howv ) ) .and.
     +       ( .not. ERMISS ( group ) ) ) THEN
	    ipowv  = NINT ( powv )
	    ihowv  = NINT ( howv )
	    igroup = NINT ( group ) 
	    IF ( ( ipowv .ge. 0 ) .and. ( ipowv .le. 99 ) .and.
     +		 ( ihowv .ge. 0 ) .and. ( ihowv .le. 99 ) .and.
     +		 ( igroup .ge. 0 ) .and. ( igroup .le. 9 ) ) THEN 
		PR_WVPH = igroup * 10000 + ipowv * 100 + ihowv
	    END IF
	END IF
C*
	RETURN
	END
