        SUBROUTINE MA_CKRP ( marrpt, igrsz, mszrpt, iret )
C************************************************************************
C* MA_CKRP							        *
C*								        *
C* This subroutine checks the length of the groups within the report.   *
C* If the length of a group is less than 3 characters or more than the  *
C* max number of characters allowed for a group within the report, then *
C* the group and all the groups that follow it are tossed out.          *
C*								        *
C* MA_CKRP  ( MARRPT, IGRSZ, MSZRPT, IRET )                             *
C*								        *
C* Input parameters:						        *
C*      MARRPT          CHAR*	        Current report                  *
C*      IGRSZ           INTEGER         Maximum size of group 		*
C*								        *
C* Input and output parameters:						*
C*      MSZRPT          INTEGER         Length of report		*
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = normal return 	        *
C*                                        1 = report rejected           *
C*								        *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       8/96   Changed wording of error message        *
C* K. Tyle/GSC		 4/97	Cleaned up				*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* D. Kidwell/NCEP      10/97   Cleaned up and improved logging         *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        LOGICAL  	more
C-----------------------------------------------------------------------
        iret  = 0
        more  = .true.
        iloc  = 0
        ict   = 0
        imxsz = igrsz + 1
C
C*      Find first blank which comes after report ID.
C
        DO WHILE ( more )
C
	    iloc = iloc + 1
	    IF ( iloc .gt. 10 ) THEN
		loglvl = 4
		logmsg = seqnum // buhd // orign // btime
		CALL DC_WLOG ( loglvl, 'MA', -2, logmsg(1:28), ier )
		CALL DC_WLOG ( loglvl, 'MA', 1, marrpt(1:mszrpt), ier )
		iret = 1
		RETURN
	    END IF
C
	    IF ( marrpt (iloc:iloc ) .eq. ' ' ) more = .false.
        END DO
C
        more = .true. 
C
        DO WHILE ( more )
	    ict = ict + 1
	    iloc = iloc + 1
	    IF ( marrpt ( iloc:iloc ) .eq. ' ' .or. 
     +           iloc .ge. mszrpt ) THEN
		IF ( ict .lt. 4 .or. ict .gt. imxsz ) THEN
C
C*		    The group is bad.  Reject the rest of the report.
C
		    loglvl = 4
		    logmsg = seqnum // buhd // orign // btime
		    CALL DC_WLOG ( loglvl, 'MA', -3, logmsg(1:28), ier )
		    CALL DC_WLOG ( loglvl, 'MA', 1, marrpt(1:mszrpt), 
     +                             ier )
		    mszrpt = iloc - ict
		    IF ( mszrpt .lt. 20 ) iret = 1
		    RETURN
		  ELSE
		    ict = 0
		END IF
	    END IF
	    IF ( iloc .ge. mszrpt ) RETURN
	END DO
C*
	RETURN
	END
