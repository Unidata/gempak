	SUBROUTINE UT_BFRI  ( iubfrp, bfmnem, rbfval, iret )
C************************************************************************
C* UT_BFRI								*
C*									*
C* This subroutine retrieves the single real value RBFVAL corresponding	*
C* to BUFR mnemonic BFMNEM from within a BUFR report.  If BFMNEM is not	*
C* found within the BUFR report (or if it contains the BUFR "missing"	*
C* value!), then RBFVAL is set to RMISSD.				*
C*									*
C* UT_BFRI  ( IUBFRP, BFMNEM, RBFVAL, IRET )				*
C*									*
C* Input parameters:							*
C*	IUBFRP		INTEGER		Logical unit number of file	*
C*					containing BUFR report		*
C*	BFMNEM		CHARACTER*(*)	BUFR mnemonic			*
C*									*
C* Output parameters:							*
C*	RBFVAL		REAL		Value corresponding to BFMNEM	*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Ator/NCEP		01/98						*
C* J. Ator/NCEP		09/00	Modify R8BFMS test			*
C* J. Ator/NCEP		06/01	Use 'BUFR.CMN' to define R8BFMS		*
C* J. Ator/NCEP		07/02	Use UT_BMRI				*
C************************************************************************
	CHARACTER*(*)	bfmnem
C*
	REAL*8		r8val
C*-----------------------------------------------------------------------
	iret = 0
C
	CALL UFBINT  ( iubfrp, r8val, 1, 1, ierufb, bfmnem )
	rbfval = UT_BMRI ( r8val )
C*
	RETURN
	END
