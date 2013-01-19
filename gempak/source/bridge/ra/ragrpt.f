	SUBROUTINE RA_GRPT  ( bultin, lenb, ibpnt, report, lenr, iret )
C************************************************************************
C* RA_GRPT								*
C*									*
C* This subroutine gets the next report from a surface bulletin.	*
C* Reports must begin with the control character, RS ( = 30 ).		*
C* 									*
C* IBPNT is assumed to be a pointer to the first character after the	*
C* end of the WMO bulletin header for the initial pass through the 	*
C* routine. The value of IBPNT is updated for subsequent passes to 	*
C* the first character after the end of the current report.		*
C*									*
C* RA_GRPT  ( BULTIN, LENB, IBPNT, REPORT, LENR, IRET )			*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		Bulletin			*
C*	LENB		INTEGER		Bulletin length			*
C*									*
C* Input and Output parameters:						*
C*	IBPNT		INTEGER		Pointer in bulletin		*
C*									*
C* Output parameters:							*
C*	REPORT		CHAR*		Report				*
C*	LENR		INTEGER		Length of report		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = no more reports		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/89	GEMPAK5					*
C* J. Whistler/SSAI	 5/91	Put definition of RS in GEMPRM.PRM	*
C* G. Rolph/NOAA/ARL	 8/95	Military SAO = separator          	*
C* S. Jacobs/NCEP	 1/96	Moved military SAO parser to RA_GMIL	*
C* S. Jacobs/NCEP	 3/96	Reworked for all SAOs without CHRS	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, report
C-----------------------------------------------------------------------
        iret = 0 
	lenr = 0
C
C*	Check for end of bulletin.
C
	IF  ( ibpnt .ge. lenb )  THEN 
	    report = ' '
            iret = -2
            RETURN
        END IF
C
C*      Find the next RS. Set the start and end points of the report.
C
	ipos = INDEX ( bultin ( ibpnt : lenb ), CHRS )
	IF  ( ipos .ne. 0 )  THEN
C
C*	    Check that there are not multiple RSs in a row. Position
C*	    the start point after the last RS.
C
	    istart = ipos + ibpnt
	    DO WHILE  ( ( istart .lt. lenb ) .and. 
     +		    	( bultin (istart:istart) .eq. CHRS ) )
		istart = istart + 1
	    END DO
C
C*	    Check for end of bulletin.
C
	    IF  ( istart .ge. lenb )  THEN
		ibpnt = lenb + 1
		report = ' '
		iret  = -2
		RETURN
	    END IF
C
C*	    Find next next RS.
C
	    iposnx = INDEX ( bultin ( istart : lenb ), CHRS )
C
C*	    Save end of report which is at end of bulletin if second
C*	    RS is not found.
C
	    IF  ( iposnx .eq. 0 )  THEN
		iend = lenb
	      ELSE
		iend = istart + iposnx - 2
	    END IF
C
C*	    Reset pointer in bulletin.
C
	    ibpnt  = iend + 1
	  ELSE
C
C*	    Check to see if this is an SAO report that
C*	    has an "=" at the end of the record instead
C*	    of an RS character at the beginning.
C
	    istart = ibpnt
	    ipos = INDEX ( bultin ( ibpnt : lenb ), '=' )
	    IF  ( ipos .eq. 0 )  THEN
		iend = lenb
	      ELSE
		iend = istart + ipos - 1
	    END IF
C
C*	    Reset pointer in bulletin.
C
	    ibpnt  = iend + 1
	    DO WHILE  ( ( ibpnt .lt. lenb ) .and. 
     +			( bultin (ibpnt:ibpnt) .eq. '=' ) )
		ibpnt = ibpnt + 1
	    END DO
	END IF
C
C*	Construct report and pointers.
C
	report = bultin  ( istart : iend )
	CALL ST_LSTR  ( report, lenin, ier )
	CALL ST_UNPR  ( report, lenin, report, lenr, ier )
C
C*	Check that this is a real report.
C
	IF  ( lenr .eq. 0 )  THEN
	    iret = -2
	END IF
C*
	RETURN
	END
