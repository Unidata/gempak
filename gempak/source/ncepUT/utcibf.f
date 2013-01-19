	SUBROUTINE UT_CIBF  ( iubfrp, bfmnem, cbfval, lcbfvl, iret )
C************************************************************************
C* UT_CIBF								*
C*									*
C* This subroutine stores the character string CBFVAL (of length up to	*
C* MXBFRR16 characters) as BUFR mnemonic BFMNEM within a BUFR report.	*
C* However, if the first character of CBFVAL is a blank, then CBFVAL is	*
C* presumed to represent a "missing" value (as in the /INTF/ format),	*
C* and is therefore not converted to BUFR.				*
C*									*
C* UT_CIBF  ( IUBFRP, BFMNEM, CBFVAL, LCBFVL, IRET )			*
C*									*
C* Input parameters:							*
C*	IUBFRP		INTEGER		Logical unit number of file	*
C*					containing BUFR report		*
C*	BFMNEM		CHARACTER*(*)	BUFR mnemonic			*
C*	CBFVAL		CHARACTER*(*)	String corresponding to BFMNEM	*
C*	LCBFVL		INTEGER		Length of CBFVAL		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Ator/NCEP		01/98						*
C* J. Ator/NCEP		06/01	Use 'BUFR.CMN'				*
C* J. Ator/NCEP		01/02	Assume CBFVAL is "missing" value if	*
C*				first character contains a blank	*
C* R. Hollern/NCEP	02/06	Added check to set length of string to 	*
C*				be converted to MXBFRR16 when length	*
C*				greater than MXBFRR16			*
C************************************************************************
	INCLUDE		'BUFR.CMN'
C*
	CHARACTER*(*)	bfmnem, cbfval
C*
	REAL*8		r8val ( MXBFLV16 )
C*-----------------------------------------------------------------------
	iret = 0
C
C*	Check for zero length.
C
	IF  ( lcbfvl .eq. 0 )  THEN
	    RETURN
	END IF
C
C*	Assume that a blank as the first character means "missing".
C
	IF  ( cbfval (1:1) .eq. ' ' )  THEN
	    RETURN
	END IF
C
C*	Set length of string to be converted.
C
        lens = lcbfvl
C
        IF ( lcbfvl .gt. MXBFRR16 ) THEN
           lens = MXBFRR16
	END IF
C
C*	Convert and store the string.
C
	CALL UT_C2R8  ( cbfval, lens, r8val, nr8val, ier2r8 )
	IF  ( ier2r8 .eq. 0 )  THEN
	    CALL UFBINT  ( iubfrp, r8val, 1, nr8val, ierufb, bfmnem )
	END IF
C*
	RETURN
	END
