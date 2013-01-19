	SUBROUTINE RS_GRPT  ( bull, lenb, maxrpt, bindex, report, lenr, 
     +			      iret )
C************************************************************************
C* RS_GRPT								*
C*									*
C*   This routine extracts a single report from a synoptic bulletin.	*
C*   It assumes that the current index is pointing to the beginning	*
C*   of a report. In this implementation, it simply looks down the	*
C*   bulletin for the first "=" character which delimits the end of	*
C*   the report.  If no "=" is found, it assumes it has a full report	*
C*   if it reaches the end of the report with more than 30 characters.	*
C*									*
C* RS_GRPT  ( BULL, LENB, MAXRPT, BINDEX, REPORT, LENR, IRET )		*
C*									*
C* Input parameters:							*
C*      BULL   		CHAR*     	The bulletin			*
C*      LENB     	INTEGER   	Length of bulletin		*
C*	MAXRPT		INTEGER		Max length of report		*
C*									*
C* Input/Output parameters:						*
C*      BINDEX   	INTEGER   	Updated pointer in bull		*
C*									*
C* Output parameters:							*
C*      REPORT   	CHAR*      	The single synoptic report	*
C*      LENR    	INTEGER 	Length of the report		*
C*      IRET		INTEGER		Return code			*
C*					  0 = Report found		*
C*					 -3 = No report found		*
C*					 -4 = Report too long (garbled)	*
C**									*
C* Log:									*
C* J. Cowie/NPS 	 7/90 	Replaces SMNXOB, SMNWOB and SMWHOB	*
C* J. Nielsen/TAMU	 2/92	Renamed from SMXRPT, made into a subr,	*
C*				Gempacized, added no "=" section	*
C************************************************************************
	CHARACTER*(*)	bull
	INTEGER		lenb, maxrpt
C*
	INTEGER		bindex
C*
	CHARACTER*(*)	report
	INTEGER		lenr, iret
C------------------------------------------------------------------------
	iret = 0
C
C*	Set maximum length for report (to screen out non-reports)
C
	imax = 250
	IF  ( maxrpt .lt. imax )  imax = maxrpt
C
C*  	Search for the first non-control character
C
	DO WHILE  ( ICHAR ( bull ( bindex:bindex ) ) .le. 32 )
	    bindex = bindex + 1 
C
C*	    Give up search if not enough space left in bull for a report
C
	    IF  ( bindex .ge. lenb-5 ) THEN
        	iret = -3
        	RETURN
            END IF
C
	END DO
C
C*  	Now, simply search for the terminator character; "=". Along the
C*  	way, convert control characters to spaces. Also, check if the
C*  	report is too big, or extending beyond the end of the bultin.
C
	lenr = 0
	DO WHILE  ( bull ( bindex:bindex ) .ne. '=' )
	    lenr = lenr + 1
	    report ( lenr:lenr ) = bull ( bindex:bindex )
	    IF  ( ICHAR ( report ( lenr:lenr ) ) .lt. 32 )  THEN
		report ( lenr:lenr ) = ' '
	    ENDIF
	    bindex = bindex + 1
	    IF  ( bindex .ge. lenb )  THEN
C
C*		If have enough characters for a report, assume we're okay
C
		IF  ( lenr .gt. 30 )  RETURN
C
C*		Not enough for a report
C
		iret = -4
		RETURN
	    ENDIF	    
	    IF  ( lenr .ge. imax ) THEN
C
C*		Too many characters; assume this is not a report
C
		iret = -4
		RETURN
	    ENDIF
	END DO
C
C*	Got it. Increment the index to just past the '=' and exit.
C
	bindex = bindex + 1
	RETURN
	END
