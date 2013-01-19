        SUBROUTINE  LS_GRPT( lenb, bulltn, jpos, lszrpt, lsfrpt, iret )
C************************************************************************
C* LS_GRPT							        *
C*								        *
C* This subroutine gets the next report in bulletin.  The length of the *
C* report must be within the range limits; otherwise, the report is     *
C* rejected.  When there are no more reports in the bulletin, IRET will *
C* be set to 2.                                                         *
C*								        *
C* LS_GRPT  ( LENB, BULLTN, JPOS, LSZRPT, LSFRPT, IRET )                *
C*								        *
C* Input parameters:						        *
C*	LENB            INTEGER         Bulletin length                 *
C*      BULLTN          CHAR*           Bulletin to decode              *
C*								        *
C* Input and Output parameters:                                         *
C*      JPOS            INTEGER         Pointer to start of report      *
C*								        *
C* Output parameters:						        *
C*      LSZRPT          INTEGER         Report length                   *
C*      LSFRPT          CHAR*           Raw report                      *
C*	IRET		INTEGER		Return code		        *
C*				   	 0 = normal return 	        *
C*                                       1 = report rejected            *
C*                                       2 = no more reports in bulletin*
C*                                       3 = found another AAXX group   *
C*								        *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      10/96  Added logic to reject NIL reports        *
C* R. Hollern/NCEP       1/98  Cleaned up and improved logging          *
C* A. Hardy/GSC	         1/98  Added GEMINC                             *
C* R. Hollern/NCEP       2/98  Redefined jsize                          *
C* D. Kidwell/NCEP	 7/02  Added check for AAXX and return code 3   *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'lscmn.cmn'
C*
        CHARACTER*(*)   bulltn, lsfrpt
C*
        LOGICAL  more
C------------------------------------------------------------------------
        iret = 0
        more = .true.
        lsfrpt = ' '
C
        DO WHILE ( more )
C
C*          Remove any spaces before start of report.
C*          Set pointer to start of report.
C
            IF ( bulltn ( jpos:jpos ) .eq. ' ' ) THEN
                jpos = jpos + 1
              ELSE
                more = .false.
            END IF
C
C*          Check for end of bulletin.
C
            jsize = lenb - jpos + 1
C
            IF ( jsize .lt. 18 ) THEN
                iret = 2
                RETURN
            END IF
        END DO
C
C*	Check for another AAXX group.
C
	IF ( bulltn ( jpos:jpos + 3 ) .eq. 'AAXX' ) THEN
	    jpos = jpos + 10
	    iret = 3
	    RETURN
	END IF
C
        lszrpt = 0
        kst = jpos
        more = .true.
C
        DO WHILE ( more )
C
C*          A report ends with the character '='.
C
            IF ( bulltn ( jpos:jpos ) .eq. '=' ) THEN
                jpos = jpos + 1
                more = .false.
              ELSE IF ( jpos .ge. lenb ) THEN
C
C*              End of bulletin reached.
C
                iret = 2
                RETURN
              ELSE
                jpos = jpos + 1
                lszrpt = lszrpt + 1
            END IF
        END DO
C
C*      Check for and reject NIL report.
C
        iipos = INDEX( bulltn ( kst:kst+10 ), 'NIL' ) 
        IF ( iipos .gt. 0 ) THEN
            iret = 1
            RETURN
        END IF
C
C*      Check that the length of the report is not too long.
C
        IF (  lszrpt .gt. 400 ) THEN
            loglvl = 4
            CALL DC_WLOG ( loglvl, 'LS', -1, bulltn(kst:kst+20), ierwl )
            iret = 1
            RETURN
        END IF
C
C*      Store report in lsfrpt.
C
        lsfrpt ( 1:lszrpt ) = bulltn ( kst:jpos )
C*
	RETURN
	END
