	SUBROUTINE UT_BFCI  ( iubfrp, bfmnem, cbfval, iret )
C************************************************************************
C* UT_BFCI								*
C*									*
C* This subroutine retrieves the character string CBFVAL corresponding	*
C* to BUFR mnemonic BFMNEM from within a BUFR report.  For this routine	*
C* to work properly, the retrieved character string must be no longer	*
C* than 8 bytes in length.  If BFMNEM is not found within the BUFR	*
C* report, then CBFVAL is set to a string of 8 blanks.			*
C*									*
C* UT_BFCI  ( IUBFRP, BFMNEM, CBFVAL, IRET )				*
C*									*
C* Input parameters:							*
C*	IUBFRP		INTEGER		Logical unit number of file	*
C*					containing BUFR report		*
C*	BFMNEM		CHARACTER*(*)	BUFR mnemonic			*
C*									*
C* Output parameters:							*
C*	CBFVAL		CHARACTER*(*)	String corresponding to BFMNEM	*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Ator/NCEP		01/98						*
C************************************************************************
	CHARACTER*(*)	bfmnem, cbfval
C*
	CHARACTER	cdata*8
C*
	REAL*8		r8val
C*-----------------------------------------------------------------------
	iret = 0
	cbfval = '        '
C
	CALL UFBINT  ( iubfrp, r8val, 1, 1, ierufb, bfmnem )
	IF  ( ierufb .eq. 1 )  THEN
	    CALL UT_R82C  ( r8val, 1, cdata, lcdata, ier82c )
	    IF  ( lcdata .gt. 0 )  THEN
		cbfval = cdata ( 1 : lcdata )
	    END IF
	END IF
C*
	RETURN
	END
