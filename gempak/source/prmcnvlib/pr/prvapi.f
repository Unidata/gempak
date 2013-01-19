	FUNCTION PR_VAPI  ( tmpc )
C************************************************************************
C* PR_VAPI								*
C*									*
C* This function computes VAPI from TMPC.  The following equation is	*
C* used when TMPC < 0.01C:						*
C*									*
C*     VAPI = 10.0 ** ( A + B + C + D )					*
C*									*
C*     A = -9.09685 * (T0 / tmpk - 1.)					*
C*     B = -3.56654 * ALOG10(T0 / tmpk)					*
C*     C = 0.87682 * (1. - tmpk / T0)					*
C*     D = 0.78614							*
C*     T0 = 273.16							*
C*									*
C* If TMPC >= 0.01C:							*
C*									*
C*     VAPI = 10.0**(A+B+C+D+ALOG10(P0))				*
C*									*
C*     A = -7.90298 * (T0 / tmpk - 1.0)					*
C*     B = 5.02808 * ALOG10(T0 / tmpk)					*
C*     C = -1.3816E-7 * (10.0**(11.344*(1.0 - tmpk / T0)) - 1.0)	*
C*     D = 8.1328E-3 * (10.0**(-3.49149*(T0 / tmpk - 1.0)) - 1.0)	*
C*     T0 = 373.16							*
C*     P0 = 1013.246							*
C*									*
C*									*
C* WMO NO-49, 2000 (Goff-Gratch equations).				*
C*									*
C* This function will compute VAPI if TMPC is input.			*
C*									*
C* REAL PR_VAPI  ( TMPC )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_VAPI		REAL		Vapor pressure in millibars	*
C**									*
C* Log:									*
C* S. Chiswell/Unidata	 1/07						*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( tmpc ) )  THEN
	    PR_VAPI = RMISSD
	  ELSE IF ( tmpc .lt. 0.01 ) THEN
	    T0 = 273.16
	    tmpk = PR_TMCK ( tmpc )
	    A = -9.09685 * ((T0 / tmpk) - 1.0) 
            B = -3.56654 * ALOG10(T0 / tmpk)
            C = 0.87682 * (1. - (tmpk / T0))
	    D = 0.78614
	    PR_VAPI = 10.0**(A + B + C + D) 
	ELSE
C
C*	    Use Goff-Gratch calculation for water when above 0.01C
C
	    T0 = 373.16
	    P0 = 1013.246
	    tmpk = PR_TMCK ( tmpc )
	    A = -7.90298 * (T0 / tmpk - 1.0)
	    B = 5.02808 * ALOG10(T0 / tmpk)
	    C = -1.3816E-7 * (10.0**(11.344*(1.0 - tmpk / T0)) - 1.0)
	    D = 8.1328E-3 * (10.0**(-3.49149*(T0 / tmpk - 1.0)) - 1.0)
	    PR_VAPI = 10.0**(A+B+C+D+ALOG10(P0))
C*
	END IF
C*
	RETURN
	END
