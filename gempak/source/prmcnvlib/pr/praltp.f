	FUNCTION PR_ALTP  ( pres, selv )
C************************************************************************
C* PR_ALTP								*
C*									*
C* This function computes altimeter from station pressure and		*
C* elevation. The following equation is used:				*
C*									*
C*     ALTI = PR_ALTI ( PRES * ( 1 - ( SELK * GAMUSD / To ) ) ** expo ) *
C*									*
C*          SELK  =  SELV / 1000					*
C*            To  =  US Std. Atmos. sea level temp in Kelvin		*
C*                =  TMCK + 15						*
C*          expo  =  - GRAVTY / ( GAMUSD * RDGAS ) * 1000		*
C*									*
C* REAL PR_ALTP  ( PRES, SELV )						*
C*									*
C* Input parameters:							*
C*	PRES		REAL	 	Station pressure in millibars	*
C*	SELV		REAL	 	Station elevation in meters	*
C*									*
C* Output parameters:							*
C*	PR_ALTP		REAL		Altimeter in inches		*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 8/90	Adapted from PR_PALT			*
C* T. Lee/GSC		12/99	Used TMCK				*
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	to = TMCK + 15. 
	IF  ( ERMISS ( pres ) .or. ERMISS ( selv ) )  THEN
	    PR_ALTP = RMISSD
	  ELSE
	    hgtk = PR_HGMK (selv)
C
C*	    Calculate the exponent.
C
	    expo =  - ( GRAVTY / ( GAMUSD * RDGAS ) * 1000.)
C
C*	    Calculate altimeter setting.
C
	    altm = pres * ( 1. - ( hgtk * GAMUSD / to ) ) ** expo
C
C*	    Convert to inches.
C
	    PR_ALTP = PR_ALTI  ( altm )
	END IF
C*
	RETURN
	END
