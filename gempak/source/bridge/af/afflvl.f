	SUBROUTINE AF_FLVL  ( cflvl, iret )
C************************************************************************
C* AF_FLVL								*
C*									*
C* This subroutine decodes and stores the flight level from AMDAR,	*
C* AIREP, and PIREP reports.						*
C*									*
C* AF_FLVL  ( CFLVL, IRET )						*
C*									*
C* Input parameters:							*
C*	CFLVL		CHAR*		Encoded	flight level 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRFLVL)	REAL		Flight level in feet  		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = error during decoding	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 7/99	Changed meters to feet in prologue      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	cflvl
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Decode the flight level.
C
	CALL AF_HHFM  ( cflvl, flvl, ierhfm )
C
	IF  ( ERMISS ( flvl ) )  THEN
	    logmsg = 'flight level '  //  cflvl
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    iret = -1
	ELSE
C
C*	    Store the flight level.
C
	    rivals ( irflvl ) = flvl
	END IF
C*
	RETURN
	END
