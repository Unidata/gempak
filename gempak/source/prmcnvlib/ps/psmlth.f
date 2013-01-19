	FUNCTION PS_MLTH ( datain, nparm )
C************************************************************************
C* PS_MLTH								*
C*									*
C* This function computes the mean mixed layer potential temperature.	*
C*									*
C*									*
C* REAL PS_MLTH ( DATAIN, NPARM )					*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of dat set parameters	*
C*									*
C* Output parameters:							*
C*	PS_MLTH		REAL		Mean mixed layer pot. temp.	*
C**									*
C* Log:									*
C* K. Brill/NMC		07/90						*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datain ( nparm, * )
C*
C------------------------------------------------------------------------
	PS_MLTH = RMISSD
C
C*	Get surface parcel values.
C
	CALL PS_PRCL  ( datain, nparm, 500., 3, 1, depth, idcord, pavg,
     +			tavg, tdavg, uavg, vavg, zavg, thavg, rmxavg,
     +			iret )
	PS_MLTH = thavg
C*
	RETURN
	END
