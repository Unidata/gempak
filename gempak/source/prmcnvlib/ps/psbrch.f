	FUNCTION PS_BRCH ( datain, nparm, tvflag )
C************************************************************************
C* PS_BRCH								*
C*									*
C* This function computes the Bulk Richardson Number:			*
C*									*
C*	BRCH = CAPE / ( 0.5 * U**2 )					*
C*									*
C*		CAPE  = Convective Available Potential Energy		*
C*		U     = magnitude of shear ( u6 - ub, v6 - vb )		*
C*		ub,vb = average u,v in the layer SFC - 500 m		*
C*		u6,v6 = average u,v in the layer SFC - 6000 m		*
C*									*
C* The first user input depth will replace the 500 m; the second will	*
C* replace the 6000 m.  TVFLAG determines if temperature or virtual	*
C* temperature is used in the calculation.				*
C*									*
C* REAL PS_BRCH ( DATAIN, NPARM, TVFLAG )				*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of dataset parameters	*
C*	TVFLAG		LOGICAL		Temperature flag		*
C*									*
C* Output parameters:							*
C*	PS_BRCH		REAL		Bulk Richardson Number		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 9/89	Adapted from PS_PRCL			*
C* M. desJardins/GSFC	 7/90	Reorganized for GEMPAK 5		*
C* J. Whistler/SSAI	 7/91	Changed ier to ier1 & 2 in PS_PRCL call	*
C* T. Lee/GSC		 8/97	Added a temperature flag		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datain ( nparm, * )
	LOGICAL		tvflag
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	PS_BRCH = RMISSD
C
C*	Compute CAPE.
C
	cape = PS_CAPE ( datain, nparm, tvflag )
	IF  ( ERMISS ( cape ) )  RETURN
C
C*	Get average u- and v- wind components in the 500 m and 6000 m
C*	layers.
C
	CALL PS_PRCL  ( datain, nparm, 500., 3, 1, depth, idcord, pavg,
     +			tavg, tdavg, u500, v500, zavg, thavg,
     +			rmxavg, ier1 )
	CALL PS_PRCL  ( datain, nparm, 6000., 3, 2, depth, idcord, pavg,
     +			tavg, tdavg, u6000, v6000, zavg, thavg,
     +			rmxavg, ier2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +	      ( ERMISS (u500) ) .or. ( ERMISS (v500) ) .or.
     +	      ( ERMISS (u6000) ) .or. ( ERMISS (v6000) ) )  RETURN
C
C*	Compute the Bulk Richardson Number.
C
	PS_BRCH = cape / ( .5 * ( (u6000 - u500) ** 2 +
     +				  (v6000 - v500) ** 2 ) )
C*
	RETURN
	END
