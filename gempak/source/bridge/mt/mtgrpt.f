	SUBROUTINE MT_GRPT  ( bultin, lenb, bultyp, ibpnt,
     +			      rpttyp, report, lenr, corflg, iret )
C************************************************************************
C* MT_GRPT							        *
C*							 	        *
C* This subroutine gets the next report from a METAR/SPECI bulletin.    *
C* Reports must begin with a four-character station id, and should end  *
C* with '='.  Reports are checked to see if they conform to the 	*
C* SAO format.  The report type is saved to common.			*
C*								        *
C* MT_GRPT  ( BULTIN, LENB, BULTYP, IBPNT, RPTTYP, REPORT, LENR,        *
C* 	      CORFLG, IRET ) 						*
C*								        *
C* Input parameters:						        *
C*	BULTIN		CHAR*		Bulletin		        *
C*	LENB		INTEGER		Bulletin length		        *
C*	BULTYP		CHAR*		Type of bulletin                *
C*								        *
C* Input and output parameters:					        *
C*	IBPNT		INTEGER		Pointer in bulletin	        *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRTHRP)	REAL		Report type flag                *
C*	RPTTYP		CHAR*		Type of report                  *
C*	REPORT		CHAR*		Report			        *
C*	LENR		INTEGER		Length of report	        *
C*	CORFLG		LOGICAL		Correction flag                 *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return	        *
C*					 -1 = no station id found       *
C*					 -2 = no more reports	        *
C*					 -3 = NIL report		*
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP     	10/95      				        *
C* D. Kidwell/NCEP 	12/95  	Check for id at start of US reports	*
C* D. Kidwell/NCEP 	 3/96	Simplified station id search          	*
C* D. Kidwell/NCEP 	 8/96	Removed references to TESTM, TESTS    	*
C* K. Tyle/GSC		11/96	Check for SAO's			 	*
C* K. Tyle/GSC		 1/97	Reorganize header and comments		*
C* K. Tyle/GSC		 3/97	Check length of station id; increase	*
C*				dimension of stid to 8			*
C* K. Tyle/GSC		 4/97	Check for NIL reports and reports that	*
C*				begin with WMO bulletin ID		*
C* D. Kidwell/NCEP 	 6/97	Removed call to ST_LSTR for stid; added *
C*				explicit character length specification *
C* D. Kidwell/NCEP 	 6/97	Cleaned up code                         *
C* D. Kidwell/NCEP 	 4/98	New interface; removed iasos flag       *
C* D. Kidwell/NCEP 	 9/05	CSC to add corflg                       *
C* S. Chiswell/Unidata	10/07	Added LWIS check			*
C************************************************************************
	INCLUDE		'mtcmn.cmn'
C*
	CHARACTER*(*)	bultin, report, bultyp, rpttyp
	LOGICAL		corflg
C*
	LOGICAL 	found, saoflg, wmo
	CHARACTER	type*8, satype*3, stid*8
C-----------------------------------------------------------------------
        iret   = 0 
	lenr   = 0
	saoflg = .false.
	wmo    = .true.
	corflg = .false.
C
C*	Check for end of bulletin.
C
        IF ( ibpnt + 7 .ge. lenb ) THEN
	    report = ' '
            iret   = -2
            RETURN
        END IF
C
C*	Get report type.
C
	rpttyp = ' '
	ibeg   = ibpnt
	type   = bultin ( ibeg:ibeg + 7 )
	indxmt = INDEX ( type, 'METAR' )
	indxsp = INDEX ( type, 'SPECI' )
	indxlw = INDEX ( type, 'LWIS' )
C
C*	Check for SAO format--found in pos. 5-6 in Canadian
C*	and 6-7 in Alaskan rpts.
C
	satype = bultin ( ibeg + 4: ibeg + 6 )
	indxsa = INDEX ( satype, 'SA' )
	indxap = INDEX ( satype, 'SP' )
	IF ( ( indxmt .ne. 0 ) .or.
     +	     ( indxlw .ne. 0 ) ) THEN
	    rpttyp = 'MT'
	  ELSE IF ( indxsp .ne. 0 ) THEN
	    rpttyp = 'SP'
	  ELSE IF ( indxsa .ne. 0 ) THEN
	    rpttyp = 'SA'
	    saoflg = .true.
	  ELSE IF ( indxap .ne. 0 ) THEN
	    rpttyp = 'SX'
	    saoflg = .true.
	END IF
C
	IF ( rpttyp .eq. ' ' ) THEN
	    rpttyp = bultyp
	    IF ( bultyp .eq. ' ' ) rpttyp = 'MT'
	  ELSE IF ( indxap .eq. 0 .and. indxsa .eq. 0 ) THEN
	    IF ( indxlw .eq. 0 ) THEN
	      indx = MAX ( indxmt, indxsp )
	      ibeg = ibeg + indx + 5
	    ELSE
	      indx = indxlw
	      ibeg = ibeg + indx + 4
	    END IF
	END IF
C
C*      Find the next station id.
C
	DO WHILE ( wmo )
	    IF ( rpttyp .ne. ' ' ) THEN
C
C*		Check length of station id.
C
		iblk = INDEX ( bultin ( ibeg:ibeg+8 ), ' ' )
		IF ( iblk .eq. 0 ) THEN 
		    stid = ' '
		  ELSE
		    stid = bultin ( ibeg:ibeg + iblk - 1 )
		END IF
		iens = ibeg + iblk
		lens = iblk - 1
		IF ( lens .eq. 4 ) THEN 
		    found = .true.
		    wmo   = .false.
		  ELSE IF ( lens .eq. 3 .and. saoflg ) THEN 
C
C*		    Canadian SAO report.
C
		    found = .true.
		    wmo   = .false.
		  ELSE 
		    found = .false.
C
C*		    Check if report is mistakenly started with the WMO
C*		    bulletin ID, or for COR preceding the station id.
C
		    IF ( lens .gt. 0 ) THEN
		        IF ( stid .eq. wmohdr ( :lens ) ) THEN
			    ibeg = ibeg + lens + 1
			  ELSE IF ( stid .eq. 'COR' ) THEN
C
C*			    COR precedes the station id (WMO 11/05).
C
			    corflg = .true.			    
			    ibeg   = ibeg + lens + 1
		          ELSE
			    wmo  = .false.
			END IF
		      ELSE IF ( lens .eq. 0 ) THEN
C
C*			Leading character is a blank, so keep looking.
C
			ibeg = ibeg + 1
		      ELSE
			wmo  = .false.
		    END IF
		END IF 
	      ELSE
		found = .false.
		wmo   = .false.
	    END IF
	END DO
C
C*      Look for '=' as report terminator.
C
	IF ( ibeg .gt. lenb ) THEN
	    iret = -2
	    RETURN
	  ELSE
	    iend1 = INDEX ( bultin ( ibeg:lenb ), '=' )
	    IF ( iend1 .ne. 0 ) THEN
		iend = iend1 + ibeg - 2
	      ELSE
		iend = lenb
	    END IF
	END IF
C
C*	Reset pointer in bulletin.
C
	ibpnt  = iend + 3
C
C*	Construct report and pointers.
C
	IF ( ibeg .gt. iend ) THEN 
	    iend = ibpnt
	    IF ( ibeg .gt. ibpnt ) THEN
		iret = -2
		RETURN
	    END IF
	END IF
	report = bultin ( ibeg : iend )
	CALL ST_LSTR ( report ( :iend - ibeg + 1 ), lenr, ier )
C
C*	Don't decode a report containing "NIL" after station ID.
C
	lenmin = MIN ( lenb, iens + 10 )
	inil   = INDEX ( bultin ( iens:lenmin ), 'NIL' )
	IF ( inil .ne. 0 ) THEN
	    iret = -3
	    CALL DC_WLOG ( 4, 'DCMETR', 6, report(:lenr), ier)
	    RETURN
	END IF
C
C*	Was a station id found?
C
	IF ( .not. found ) THEN
C
C*	    No station id found where expected.
C
	    iret = -1
	  ELSE
C
C*	    Have a four character alpha string - assume station id.  
C*	    Initialize common variables to missing.
C
	    CALL MT_INIT ( mret )
C
C*	    Store report type.
C
	    IF ( rpttyp .eq. 'MT' ) THEN
		rivals ( irthrp ) = 0.
	      ELSE IF ( rpttyp .eq. 'SP' ) THEN
		rivals ( irthrp ) = 1.
	    END IF
	END IF
C*
	RETURN
	END
