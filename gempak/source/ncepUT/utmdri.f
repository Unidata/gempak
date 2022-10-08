	REAL FUNCTION UT_MDRI  ( fmval )
C************************************************************************
C* UT_MDRI								*
C*									*
C* Given a real value that was output by the MADIS (Meteorological	*
C* Assimilation	Data Ingest System) software from FSL, this function	*
C* returns it as a real interface value.  If the MADIS "missing" value	*
C* is input, then RMISSD is returned.					*
C*									*
C* UT_MDRI ( RMVAL )							*
C*									*
C* Input parameters:							*
C*	RMVAL		REAL		MADIS value			*
C*									*
C* Output parameters:							*
C*	UT_MDRI		REAL		Interface value			*
C**									*
C* Log:									*
C* J. Ator/NCEP		06/01						*
C* J. Ator/NCEP		09/01	Modify to use FSL MADIS software	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( FMMISS = 999999.0 )
C*-----------------------------------------------------------------------
	iret = 0
C
	IF  ( fmval .ne. FMMISS )  THEN
	    UT_MDRI = fmval
	ELSE
	    UT_MDRI = RMISSD
	END IF
C*
	RETURN
	END
