	SUBROUTINE DC_IFFP  ( mnem, imnem, nimn, allok, iloc, iret )
C************************************************************************
C* DC_IFFP								*
C*									*
C* This subroutine searches for a specified mnemonic MNEM within a	*
C* specified interface mnemonic array.  If found, then the relative	*
C* position within the array is returned in ILOC.  Otherwise, the	*
C* logical variable ALLOK, which keeps track of whether all mnemonics	*
C* searched for during all calls to this subroutine have been found,	*
C* is set to .false.							*
C*									*
C* DC_IFFP  ( MNEM, IMNEM, NIMN, ALLOK, ILOC, IRET )			*
C*									*
C* Input parameters:							*
C*	MNEM		CHAR*		Mnemonic			*
C*      IMNEM (*)	CHAR*		Interface mnemonic array        *
C*	NIMN		INTEGER		Number of mnemonics in array    *
C*									*
C* Input and output parameters:						*
C*	ALLOK		LOGICAL		"Mnemonic not found" alert flag *
C*									*
C* Output parameters:							*
C*	ILOC		INTEGER		Relative position of MNEM       *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP          8/97                                           *
C* D. Kidwell/NCEP	12/97	Adapted and generalized from AF_IFFP    *
C************************************************************************
	CHARACTER*(*)	mnem, imnem(*)
	LOGICAL		allok
C*
	CHARACTER	logmsg*30
C-----------------------------------------------------------------------
	iret = 0
	iloc = 0
C
	CALL ST_FIND  ( mnem, imnem, nimn, iloc, ier )
C
	IF  ( iloc .eq. 0 )  THEN
	    allok = .false.
	    logmsg = mnem // ' in interface arrays.'
	    CALL DC_WLOG  ( 0, 'DC', -14, logmsg, ierwlg )
	END IF
C*
	RETURN
	END
