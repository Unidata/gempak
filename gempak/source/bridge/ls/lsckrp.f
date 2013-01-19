        SUBROUTINE  LS_CKRP ( lsfrpt, igrsz, lszrpt, iret )
C************************************************************************
C* LS_CKRP							        *
C*								        *
C* This subroutine checks the length of the groups within the report.   *
C* If the length of a group is less than 3 characters or more than the  *
C* max number of characters allowed for a group within the report, then *
C* the group and all the groups that follow it are tossed out.          *
C*								        *
C* LS_CKRP  ( LSFRPT, IGRSZ, LSZRPT, IRET )                             *
C*								        *
C* Input parameters:						        *
C*      LSFRPT          CHAR*           Current report                  *
C*      IGRSZ           INTEGER         Maximum size of group           *
C*								        *
C* Input and Output parameters:                                         *
C*      LSZRPT          INTEGER         Report length                   *
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*				   	 0 = normal return 	        *
C*                                       1 = report rejected            *
C** 								        *
C* Log:								        *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       8/96   Changed wording of error message        *
C* R. Hollern/NCEP       1/98   Cleaned up and improved logging         *
C* A. Hardy/GSC          1/98   Reordered calling sequence, added GEMINC*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        LOGICAL  more
C------------------------------------------------------------------------
        iret = 0
        more = .true.
        iloc = 0
        ict = 0
        imxsz = igrsz + 1
C
C*      Find first blank which comes after report ID.
C
        DO WHILE ( more )
C
            iloc = iloc + 1
            IF ( iloc .gt. 10 ) THEN
                loglvl = 2
                logmsg = seqnum // buhd // orign // btime
                CALL DC_WLOG( loglvl, 'LS', -2, logmsg ( 1:28 ), ier )
                CALL DC_WLOG( loglvl, 'LS', 1, lsfrpt (1:lszrpt), ier )
                iret = 1
                RETURN
            END IF
C
            IF ( lsfrpt ( iloc:iloc ) .eq. ' ' ) more = .false.
        END DO
C
        more = .true. 
C
        DO WHILE ( more )
            ict = ict + 1
            iloc = iloc + 1
            IF ( lsfrpt ( iloc:iloc ) .eq. ' ' .or. 
     +           iloc .ge. lszrpt ) THEN
                IF ( ict .lt. 4 .or. ict .gt. imxsz ) THEN
C
C*                  This group is bad. Reject the rest of the report.
C
                    loglvl = 4
                    logmsg = seqnum // buhd // orign // btime
                    CALL DC_WLOG( loglvl, 'LS', -3, logmsg(1:28), ier )
                    CALL DC_WLOG( loglvl, 'LS', 1, lsfrpt (1:lszrpt),
     +                            ier )
                    lszrpt = iloc - ict
                    IF ( lszrpt .lt. 20 ) iret = 1
                    RETURN
                  ELSE
                    ict = 0
                END IF
            END IF
            IF ( iloc .ge. lszrpt ) RETURN
        END DO
C*
	RETURN
	END
