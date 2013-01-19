	FUNCTION PR_PMSL  ( pres, tmpc, dwpc, selv )
C************************************************************************
C* PR_PMSL								*
C*									*
C* This function computes PMSL from PRES, TMPC, DWPC, and SELV.  The	*
C* following equation is used:						*
C*									*
C*    PMSL = PRES * EXP ( ( GRAVTY * SELV ) / ( RDGAS * TVAVE ) )	*
C*									*
C*         TVAVE = avg virtual temp between station and sea level	*
C*               = TVRK + ( DELTV / 2 )					*
C*         DELTV = GAMUSD * SELV / 1000					*
C*									*
C* Wallace and Hobbs.							*
C*									*
C* REAL PR_PMSL  ( PRES, TMPC, DWPC, SELV )				*
C*									*
C* Input parameters:							*
C*	PRES		REAL	 	Station pressure in millibars	*
C*	TMPC		REAL	 	Temperature in Celsius		*
C*	DWPC		REAL	 	Dewpoint in Celsius		*
C*	SELV		REAL	 	Station elevation in meters	*
C*									*
C* Output parameters:							*
C*	PR_PMSL		REAL		Mean sea level pressure	in mb	*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84	Original source				*
C* G. Huffman/GSC       7/88	Revise 					*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing inputs.
C
	IF  ( ERMISS ( pres ) .or. ERMISS ( tmpc ) .or.
     +	      ERMISS ( dwpc ) .or. ERMISS ( selv ) )  THEN
	    PR_PMSL = RMISSD
	    RETURN
	END IF
C
C*	Calculate virtual temperature.
C
	tv = PR_TVRK  ( tmpc, dwpc, pres )
C
C*	Deltv and tvave.
C
	deltv = selv * GAMUSD / 1000.
	tvave = tv + ( deltv / 2. )
C
C*	Compute.
C
	PR_PMSL = pres * EXP ( ( GRAVTY * selv ) / (RDGAS * tvave) )
C*
	RETURN
	END
