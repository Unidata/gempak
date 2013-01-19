	SUBROUTINE TF_GRPT  ( bull, lenb, iptr, rpt, lenr, stid, cntry,
     +			      iret )
C************************************************************************
C* TF_GRPT							        *
C*							 	        *
C* This subroutine gets the next report from a TAF bulletin, and also   *
C* returns the station id.  Reports must begin with a four-character    *
C* station id and end with '='.                                         *
C*								        *
C* TF_GRPT  ( BULL, LENB, IPTR, RPT, LENR, STID, CNTRY, IRET ) 		*
C*								        *
C* Input parameters:						        *
C*	BULL		CHAR*		Partial bulletin	        *
C*	LENB		INTEGER		Partial bulletin length	        *
C*								        *
C* Output parameters:						        *
C*	IPTR		INTEGER		Pointer in bull. to end of rpt. *
C*	RPT		CHAR*		Report			        *
C*	LENR		INTEGER		Length of report	        *
C*	STID		CHAR*		TAF station id                  *
C*	CNTRY		CHAR*		Country code                    *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return	        *
C*					 -1 = no station id found       *
C*					 -3 = NIL report		*
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP 	 9/02	                                        *
C* D. Kidwell/NCEP 	 9/02	Added argument ostid                    *
C* D. Kidwell/NCEP 	10/02	Bug fixes for ibeg and iptr definitions *
C* A. Hardy/NCEP	12/02   Removed stripping of stn id's 'K' & 'C' *
C* D. Kidwell/NCEP 	 5/03	Removed argument ostid, added cntry     *
C************************************************************************
	CHARACTER*(*)	bull, rpt, stid, cntry
C*
	LOGICAL 	found
	CHARACTER	string*30, carr (15)*5
C-----------------------------------------------------------------------
        iret  = 0 
	rpt   = ' '
	lenr  = 0
	stid  = ' '
	cntry = ' '
C
C*	Break the first part of the partial bulletin into words.  If it
C*	is not the end of the bulletin, make sure to end on a blank.
C
	lens   = MIN0 ( lenb, 30 )
	IF ( lenb .gt. lens ) THEN
	    found = .false.
	    lblnk = lens
	    DO WHILE ( .not. found ) 
		IF ( bull ( lblnk:lblnk ) .eq. ' ' ) THEN
		    found = .true.
		    lens  = lblnk
		  ELSE
		    lblnk = lblnk - 1
		    IF ( lblnk .eq. 0 )  found = .true.
		END IF
	    END DO
	END IF
	string = bull ( :lens )
	CALL ST_CLST ( string, ' ', ' ', 15, carr, num, ier )
C
C*      Look for the next station id.
C
	found = .false.
	iw    = 1
	DO WHILE ( .not. found )
	    CALL ST_LSTR ( carr ( iw ), lenc, ier )
	    IF ( lenc .eq. 4 ) THEN
		CALL ST_WORD ( carr ( iw ), ityp, ier )
		IF ( ityp .eq. 0 ) THEN
C
C*		    Station id was found.
C
		    ibeg  = INDEX ( string, carr ( iw ) ) 
		    found = .true.
		    stid  = carr ( iw )
		    IF ( ( stid ( 1:1 ) .eq. 'K' ) .or.
     +			 ( stid ( 1:1 ) .eq. 'P' ) .or.
     +			 ( ( stid ( 1:1 ) .eq. 'T' ) .and.
     +			   ( ( stid ( 2:2 ) .eq. 'I' ) .or.
     +			     ( stid ( 2:2 ) .eq. 'J' ) .or.
     +			     ( stid ( 2:2 ) .eq. 'K' ) .or.
     +			     ( stid ( 2:4 ) .eq. 'NCM' ) ) ) .or.
     +			 ( stid ( 1:4 ) .eq. 'NSTU' ) ) THEN
      			cntry = 'US'
		    END IF
		END IF
	    END IF
	    IF ( .not. found ) THEN
		iw = iw + 1
		IF ( iw .gt. num ) THEN
		    iptr = lens
		    iret = -1
		    RETURN
		END IF
	    END IF
	END DO
C
C*      Look for '=' as report terminator.
C
	ieql = INDEX ( bull ( ibeg:lenb ), '=' )
	IF ( ieql .ne. 0 ) THEN
	    iend = ieql + ibeg - 2
	  ELSE
	    iend = lenb
	END IF
C
C*	Reset pointer in bulletin to end of report.
C
	iptr = iend + 1
C
C*	Construct report and pointers.
C
	rpt = bull ( ibeg:iend )
	CALL ST_LSTR ( rpt ( :iend - ibeg + 1 ), lenr, ier )
C
C*	Check for a report containing only " NIL".
C
	IF ( lenr .le. 25 ) THEN
	    IF (  INDEX ( rpt ( :lenr ), ' NIL' ) .ne. 0 ) THEN
	        iret = -3
	    END IF
	END IF
C*
	RETURN
	END
