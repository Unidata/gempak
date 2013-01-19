	FUNCTION PS_BVFQ  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_BVFQ								*
C*									*
C* This function computes the Brunt-Vaisala Frequency.			*
C*									*
C* REAL PS_BVFQ  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_BVFQ		REAL		Brunt-Vaisala Frequency		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datain (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
C*	Get Brunt-Vaisala Frequency squared.
C
	bvfqsq = PS_BVSQ  ( datain, nparm, clev, ivcord )
C
C*	The Brunt-Vaisala Frequency is the square root of this number.
C
	IF  ( ERMISS ( bvfqsq ) )  THEN
	    PS_BVFQ = RMISSD
	  ELSE
	    PS_BVFQ = SQRT ( bvfqsq )
	END IF
C*
	RETURN
	END
