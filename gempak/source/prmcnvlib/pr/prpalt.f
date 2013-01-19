	FUNCTION PR_PALT  ( altm, selv )
C************************************************************************
C* PR_PALT								*
C*									*
C* This function computes station pressure from altimeter and station	*
C* elevation.  The following equation is used:				*
C*									*
C*     PALT = ALTM * ( 1 - ( SELK * GAMUSD / To ) ) ** expo		*
C*									*
C*          SELK  =  SELV / 1000					*
C*            To  =  US Std. Atmos. sea level temp in Kelvin		*
C*                =  TMCK + 15						*
C*          expo  =  GRAVTY / ( GAMUSD * RDGAS ) * 1000			*
C*									*
C* Wallace and Hobbs.							*
C*									*
C* REAL PR_PALT  ( ALTM, SELV )						*
C*									*
C* Input parameters:							*
C*	ALTM		REAL	 	Altimeter in millibars		*
C*	SELV		REAL	 	Station elevation in meters	*
C*									*
C* Output parameters:							*
C*	PR_PALT		REAL		Pressure in millibars		*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84	Original source				*
C* I. Graffman/RDS	12/87	Use constants from table		*
C* G. Huffman/GSC	 7/88	Documentation				*
C* T. Lee/GSC		12/99	Used TMCK				*
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	to = TMCK + 15.
	IF  ( ERMISS ( altm ) .or. ERMISS ( selv ) )  THEN
	    PR_PALT = RMISSD
	  ELSE
	    hgtk = PR_HGMK (selv)
C
C*	    Calculate the exponent.
C
	    expo = ( GRAVTY / ( GAMUSD * RDGAS ) * 1000.)
C
C*	    Calculate pressure.
C
	    PR_PALT = altm * ( 1. - ( hgtk * GAMUSD / to ) ) ** expo
	END IF
C*
	RETURN
	END
