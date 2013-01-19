	FUNCTION PR_PLCL  ( tmpc, pres, tlcl )
C************************************************************************
C* PR_PLCL 								*
C*									*
C* This function computes PLCL from TMPC, PRES, and TLCL for an air	*
C* parcel.  TMPC and PRES refer to the parcel before lifting, while	*
C* TLCL is the temperature at the LCL.  TLCL may be computed using	*
C* PR_TLCL.  The equation used is a modified Poisson equation:		*
C*									*
C*          PLCL = PRES * ( TLCL / TMPK ) ** ( 1 / RKAPPA )		*
C*									*
C* REAL PR_PLCL  ( TMPC, PRES, TLCL )					*
C*									*
C* Input parameters:							*
C*	TMPC		REAL    	Temperature in Celsius		*
C*	PRES		REAL		Pressure in millibars		*
C*	TLCL  		REAL    	LCL temperature in Kelvin	*
C*									*
C* Output parameters:							*
C*	PR_PLCL		REAL		LCL pressure in millibars	*
C**									*
C* Log:									*
C* M. Goodman/RDS	8/84	Original source				*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	7/88	Documentation				*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( pres ) .or. ERMISS ( tmpc ) .or.
     +	      ERMISS ( tlcl ) )  THEN
	    PR_PLCL = RMISSD
	  ELSE
	    tmpk = PR_TMCK (tmpc)
	    PR_PLCL = pres * (tlcl / tmpk) ** (1 / RKAPPA)
	END IF
C*
	RETURN
	END
