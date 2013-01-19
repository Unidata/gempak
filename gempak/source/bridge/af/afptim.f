	SUBROUTINE AF_PTIM  ( report, istim, ietim, iret )
C************************************************************************
C* AF_PTIM								*
C*									*
C* This subroutine decodes and stores the time data from within a	*
C* PIREP report.							*
C*									*
C* AF_PTIM  ( REPORT, ISTIM, IETIM, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISTIM		INTEGER		Pointer to start of time data	*
C*					within REPORT 			*
C*	IETIM		INTEGER		Pointer to end of time data	*
C*					within REPORT 			*
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
	itstim = istim
C
	CALL AF_GFLD  ( report, ietim, itstim, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 4 )  THEN
	    CALL AF_HHMM  ( field (1:4), iertim )
	END IF
C*
	RETURN
	END
