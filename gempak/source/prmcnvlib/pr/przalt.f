	FUNCTION PR_ZALT  ( altm, pres )
C************************************************************************
C* PR_ZALT								*
C*									*
C* This function computes station elevation from altimeter and station	*
C* pressure. It is also used to estimate height at various pressure	*
C* levels from the altimeter in millibars.  The PC library computes	*
C* ZMSL, Z000, Z950, Z850, Z800 by calling this function with PRES	*
C* equal to PMSL, 1000, 950, 850 and 800 respectively.  The following	*
C* equation is used:							*
C*									*
C*    ZALT  =  [ To * ( 1 - ( ( PRES/ALTM ) ** expo ) ] / GAMMA		*
C*									*
C*          GAMMA  = GAMUSD / 1000					*
C*             To  =  US Std. Atmos. sea level temp in Kelvin		*
C*                 =  TMCK + 15						*
C*           expo  =  ( GAMMA * RDGAS ) / GRAVTY			*
C*									*
C* REAL PR_ZALT  ( ALTM, PRES )						*
C*									*
C* Input parameters:							*
C*	ALTM		REAL	 	Altimeter in millibars		*
C*	PRES		REAL	 	Pressure in millibars		*
C*									*
C* Output parameters:							*
C*	PR_ZALT		REAL	 	Height in meters		*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84	original source				*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Documentation; check ALTM, PRES .le. 0	*
C* T. Lee/GSC		12/99	Used TMCK				*
C************************************************************************
        INCLUDE  	'GEMPRM.PRM'
        INCLUDE  	'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for bad data.
C
	IF  ( ERMISS ( altm ) .or. ERMISS ( pres )
     +       .or. ( altm .le. 0. ) .or. ( pres .le. 0. ) )  THEN
	    PR_ZALT = RMISSD
	    RETURN
	END IF
C
C*	Set constants.
C
	to    = TMCK + 15.
	gamma = GAMUSD / 1000.
C
C*	Calculate the exponent and pressure ratio.
C
	expo =  ( gamma * RDGAS ) / GRAVTY 
	prat = pres / altm
C
C*	Calculate.
C
	PR_ZALT = ( to * ( 1. - prat ** expo ) ) / gamma
C*
	RETURN
	END
