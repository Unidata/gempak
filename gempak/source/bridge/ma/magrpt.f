	SUBROUTINE  MA_GRPT( lenb, bulltn, jpos, mszrpt, marrpt, iret )
C************************************************************************
C* MA_GRPT							        *
C*								        *
C* This subroutine gets the next non-Coast Guard report in bulletin.	*
C* The length of the report must be within the range limits; otherwise,	*
C* the report is rejected.  When there are no more reports in the	*
C* bulletin, IRET will be set to 2.					*
C*								        *
C* MA_GRPT  ( LENB, BULLTN, JPOS, MSZRPT, MARRPT, IRET )                *
C*								        *
C* Input parameters:						        *
C*	LENB            INTEGER         Length of bulletin in bytes     *
C*      BULLTN          CHAR*	        Raw bulletin to process         *
C*									*
C* Input and output parameters:						*
C*      JPOS            INTEGER         Points to start of report on    *
C*                                      input, to next report on output *
C*								        *
C* Output parameters:						        *
C*      MSZRPT          INTEGER         Length of report		*
C*      MARRPT          CHAR*	        Report to process               *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = normal return 	        *
C*                                        1 = report rejected           *
C*                                        2 = no more reports		*
C*					 -1 = NIL report - rejected     *
C*								        *
C** 								        *
C* Log:								        *
C* R. Hollern/NCEP       6/96                                           *
C* K. Tyle/GSC		 4/97	Cleaned up				*
C* R. Hollern/NCEP      10/97   Added test to reject 'NIL' reports      *
C* D. Kidwell/NCEP      10/97   Cleaned up and improved logging         *
C* R. Hollern/NCEP      12/97   Removed code to write report to LOG file*
C* F. J. Yen/NCEP	 4/01	Modified prologue about non-Coast Guard *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   bulltn, marrpt
C*
        CHARACTER	chrstr*1
        LOGICAL  	more
C-----------------------------------------------------------------------
        iret   = 0
        more   = .true.
	marrpt = ' '
C
        DO WHILE ( more )
C
C*          Remove any spaces before start of report.
C*	    Set pointer to start of report.
C
	    IF ( bulltn ( jpos:jpos ) .eq. ' ' ) THEN
		jpos = jpos + 1
	      ELSE
		more = .false.
	    END IF
C
C*          Check for end of bulletin.
C
            jsize = lenb - jpos
            IF ( jsize .lt. 30) THEN
		iret = 2
		RETURN
	    END IF
	END DO
C
        mszrpt = 0
        kst    = jpos
        more   = .true.
C
        DO WHILE ( more )
C
C*          A report normally ends with the character '='.
C
	    IF ( bulltn ( jpos:jpos ) .eq. '=' ) THEN
		jpos = jpos + 1
		more = .false.
	      ELSE IF ( bulltn ( jpos:jpos+3 ) .eq. 'BBXX' ) THEN
		jpos = jpos + 4
		more = .false.
	      ELSE
		jpos = jpos + 1
		mszrpt = mszrpt + 1
		IF ( jpos .ge. lenb ) THEN
C
C*		    Report does not end with '='. 
C
		    more = .false.
		END IF
	    END IF
	END DO
C
C*      Check for SHXX bulletin.  Some reports are not preceded
C*      by BBXX ID.  This can cause the merging of two reports.
C
        IF ( buhd ( 1:4 ) .eq. 'SHXX' ) THEN
	    IF ( mszrpt .gt. 150 ) THEN
C
C*	    Search for start of second report in marrpt
C*	    which will be identified by alpha string.
C
		more = .true.
		j = kst + 30
		DO WHILE ( more )
		    j = j + 1
		    chrstr = bulltn ( j:j )
		    CALL ST_ALNM ( chrstr, ityp, mret )
		    IF ( ityp .eq. 2 ) THEN
C
C*			Chrstr is a letter.  Will assume start
C*			of ship ID.
C
			IF ( bulltn (j:j+2 ) .ne. 'ICE' ) THEN
			    mszrpt = j - kst
			    more = .false.
			    jpos = j - 1
			  ELSE
			    j = j + 3
			END IF
		    END IF
		END DO
	    END IF
        END IF
C
C*	Check for and reject NIL report.
C
	IF ( INDEX ( bulltn ( kst:kst+10 ), 'NIL' ) .gt. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*      Check that length of report is neither too long nor too short.
C
        IF ( mszrpt .lt. 23 .or. mszrpt .gt. 400 ) THEN
	    loglvl = 4
	    CALL DC_WLOG( loglvl, 'MA', -1, bulltn(kst:kst+20), ierwlg )
            iret = 1
            RETURN
        END IF
C
C*      Store report in marrpt.
C
        marrpt ( 1:mszrpt ) = bulltn ( kst:jpos )
C*
	RETURN
	END
