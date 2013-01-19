	SUBROUTINE AF_PFLV  ( report, isflv, ieflv, iret )
C************************************************************************
C* AF_PFLV								*
C*									*
C* This subroutine decodes and stores the flight level data from within	*
C* a PIREP report.							*
C*									*
C* AF_PFLV  ( REPORT, ISFLV, IEFLV, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISFLV		INTEGER		Pointer to start of flight level*
C*					data within REPORT 		*
C*	IEFLV		INTEGER		Pointer to end of flight level	*
C*					data within REPORT 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Remove calls to ERRRPT 			*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	field*(MXLENF)
C-----------------------------------------------------------------------
	iret = 0
	itsflv = isflv
C
	CALL AF_GFLD  ( report, ieflv, itsflv, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 3 )  THEN
	    CALL AF_FLVL  ( field (1:3), ierflv )
	END IF
C*
	RETURN
	END
