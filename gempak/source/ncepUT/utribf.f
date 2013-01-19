	SUBROUTINE UT_RIBF  ( iubfrp, bfmnem, rbfval, iret )
C************************************************************************
C* UT_RIBF								*
C*									*
C* This subroutine stores the single real value RBFVAL as BUFR mnemonic	*
C* BFMNEM within a BUFR report.						*
C*									*
C* UT_RIBF  ( IUBFRP, BFMNEM, RBFVAL, IRET )				*
C*									*
C* Input parameters:							*
C*	IUBFRP		INTEGER		Logical unit number of file	*
C*					containing BUFR report		*
C*	BFMNEM		CHARACTER*(*)	BUFR mnemonic			*
C*	RBFVAL		REAL		Value corresponding to BFMNEM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Ator/NCEP		01/98						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bfmnem
C*
	REAL*8		r8val
C*
	INCLUDE		'ERMISS.FNC'
C*-----------------------------------------------------------------------
	iret = 0
C
	IF  ( .not. ERMISS ( rbfval ) )  THEN
	    r8val = rbfval
	    CALL UFBINT  ( iubfrp, r8val, 1, 1, ierufb, bfmnem )
	END IF
C*
	RETURN
	END
