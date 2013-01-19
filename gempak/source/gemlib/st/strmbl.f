	SUBROUTINE ST_RMBL  ( string, outstr, length, iret )
C************************************************************************
C* ST_RMBL								*
C*									*
C* This subroutine removes spaces and tabs from a string.  The input	*
C* and output strings may be the same variable.				*
C*									*
C* ST_RMBL  ( STRING, OUTSTR, LENGTH, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String without blanks		*
C*	LENGTH		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/84						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/NMC	 3/92	Add temporary variable ttt		*
C* L. Sager/NCEP         2/96   Increased size of sss and ttt to 160    *
C* D. Kidwell/NCEP       6/97   Referenced actual length of input string*
C* T. Piper/SAIC	12/07	Correct output length calculation	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	chr*1, sss*160, ttt*160
C-----------------------------------------------------------------------
	iret   = 0
	length = 0
	lenin  = LEN ( string )
	lenst  = MIN ( lenin, 160 )
	sss    = string
	ttt    = ' '
C
C*	Get length of input string.
C
	CALL ST_LSTR  ( sss(:lenst), lens, iret )
C
C*	Check each character to see if it is a blank.
C
	DO  ichr = 1, lens
	    chr = sss(ichr:ichr)
	    IF  ( ( chr .ne. CHSPAC ) .and. ( chr .ne. CHTAB ) )  THEN
		length = length + 1
		ttt(length:length) = chr
	    END IF
	END DO
C*
	length = MIN ( length, LEN(outstr) )
	outstr = ttt
C*
	RETURN
	END
