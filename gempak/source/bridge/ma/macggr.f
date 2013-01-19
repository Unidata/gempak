	SUBROUTINE  MA_CGGR( lenb, bulltn, jpos, mszrpt, cgrpt, iret )
C************************************************************************
C* MA_CGGR							        *
C*								        *
C* This subroutine gets the next Coast Guard report in bulletin.  The	*
C* length of the report must be within the range limits; otherwise, the	*
C* report is rejected.  When there are no more reports in the bulletin,	*
C* IRET will be set to 2.						*
C*								        *
C* MA_CGGR  ( LENB, BULLTN, JPOS, MSZRPT, CGRPT, IRET )                 *
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
C*      CGRPT           CHAR*	        Report to process               *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = normal return 	        *
C*                                        1 = report rejected           *
C*                                        2 = no more reports		*
C*								        *
C** 								        *
C* Log:								        *
C* C. Caruso Magee/NCEP	 4/01	Modifying for Coast Guard data.         *
C* F. J. Yen/NCEP	 4/01	Cleaned up, reformatted, and renamed	*
C*				from CG_GRPT.				*
C************************************************************************
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   bulltn, cgrpt
C*
        CHARACTER       carrtn
        LOGICAL  	more
C-----------------------------------------------------------------------
        iret   = 0
        more   = .true.
        carrtn = CHAR(13)
	cgrpt = ' '
        mszrpt = 0
        kst    = jpos
        more   = .true.
C
        DO WHILE ( more )
	    jpos = jpos + 1
	    mszrpt = mszrpt + 1
C
C*          Read until you hit a cr-cr
C               
            IF ( bulltn(jpos-1:jpos-1) .eq. carrtn .and.
     +               bulltn(jpos:jpos) .eq. carrtn ) THEN
C
C*              Subtract 1 off of report size.  Don't want cr-cr included.
C
		mszrpt = mszrpt - 1
                more = .false.
            END IF

	    IF ( jpos .ge. lenb ) THEN
C
C*	        Reached end of bulletin. 
C
	        more = .false.
                iret = 2
                RETURN
	    END IF
	END DO
C
C*      Check that length of report is neither too long nor too short.
C
        IF ( mszrpt .lt. 23 .or. mszrpt .gt. 400 ) THEN
	    loglvl = 4
	    CALL DC_WLOG( loglvl, 'MA', -1, bulltn(kst:kst+10), ierwlg )
            iret = 1
            RETURN
        END IF
C
C*      Store report in cgrpt.
C
        cgrpt ( 1:mszrpt ) = bulltn ( kst:jpos )
C*
	RETURN
	END
