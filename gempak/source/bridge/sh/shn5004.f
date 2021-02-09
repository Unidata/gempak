	SUBROUTINE SHN_5004  ( nmin, iret )
C************************************************************************
C* SHN_5004								*
C*									*
C* This subroutine determines the number of minutes elapsed between	*
C* the current report date-time and the previous 7:00am local time	*
C* at the location corresponding to the current report.			* 
C*									*
C* SHN_5004 ( NMIN, IRET )						*
C*									*
C* Output parameters:							*
C*	NMIN		INTEGER		Number of minutes elapsed	*
C*					between current report date-time*
C*					and previous 7:00am local time	*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C*					 -1 = unable to determine NMIN	*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C* S. Guan/NCEP		07/20	Added tzone as an input of TI_DST       *  
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C*
	INTEGER		idtarr (5), jdtarr (5)
C*
        CHARACTER       tzone*3
C*
	LOGICAL		dst
C-----------------------------------------------------------------------
	iret = -1 
C
C*      Determine timezone
C 
        idst = 0
        CALL SHN_DFHR ( idst, rivals (6), rivals (7), idiff, ierdhr )
        IF ( ierdhr .ne. 0 ) THEN
            RETURN
        END IF
C*
        IF ( idiff .eq. -5 ) THEN
            tzone = 'E' 
        ELSE IF ( idiff .eq. -6 ) THEN
            tzone = 'C'
        ELSE IF ( idiff .eq. -7 ) THEN
            tzone = 'M'
        ELSE IF ( idiff .eq. -8 ) THEN
            tzone = 'P' 
        ELSE
C*          Outside CONUS zones, default to Eastern for initial
C*          TI_DST call only (preserves legacy TI_DST behavior)
            tzone = 'E' 
        END IF
           
C
C*	Determine whether the current report date-time occurs during
C*	Daylight Savings Time.
C
	idtarr (1) = INT ( rivals (1) )
	idtarr (2) = INT ( rivals (2) )
	idtarr (3) = INT ( rivals (3) )
	idtarr (4) = INT ( rivals (4) )
	idtarr (5) = INT ( rivals (5) )
        CALL TI_DST  ( idtarr, tzone, dst, ierdst ) 
	IF ( ierdst .ne. 0 ) THEN
	    CALL UT_EMSG ( 2, 'TI_DST', ierdst )
	    RETURN
	ELSE IF ( dst ) THEN
	    idst = 1
	ELSE
	    idst = 0
	END IF
C
C*	Now, using the location corresponding to the current report,
C*	determine the difference (in hours) between GMT and the local
C*	time at that location.
C
	CALL SHN_DFHR ( idst, rivals (6), rivals (7), idiff, ierdhr )
	IF ( ierdhr .ne. 0 ) THEN
	    RETURN
	END IF
C
C*	Now, convert the current report date-time (which is in GMT!)
C*	to local time.
C
	CALL TI_SUBM ( idtarr, idiff * (-60), idtarr, iersbm )
	IF ( iersbm .ne. 0 ) THEN
	    CALL UT_EMSG ( 2, 'TI_SUBM', iersbm )
	    RETURN
	END IF
C
C*	Now, given that we now know the current report date-time in
C*	terms of local time at the given location, determine the
C*	date-time corresponding to the previous 7:00am local at the same
C*	location.
C
	jdtarr (1) = idtarr (1)
	jdtarr (2) = idtarr (2)
	jdtarr (3) = idtarr (3)
	jdtarr (4) = 7
	jdtarr (5) = 0
	IF ( ( idtarr (4) .lt. 7 ) .or.
     +	    ( ( idtarr (4) .eq. 7 ) .and. ( idtarr (5) .eq. 0 ) ) ) THEN
	    CALL TI_SUBD ( jdtarr, jdtarr, iersbd )
	    IF ( iersbd .ne. 0 ) THEN
	        CALL UT_EMSG ( 2, 'TI_SUBD', iersbd )
	        RETURN
	    END IF
	END IF
C
C*	Finally, determine the elapsed time (in minutes) between the
C*	current report date-time and the previous 7:00am local.
C
	CALL TI_MDIF ( idtarr, jdtarr, nmin, iermdf )
	IF ( iermdf .ne. 0 ) THEN
	    CALL UT_EMSG ( 2, 'TI_MDIF', iermdf )
	    RETURN
	END IF
C
	iret = 0
C*
	RETURN
	END
